## Fabfile for releasing R packages
## 15 Jan 2013
## Neal Richardson
## neal@crunch.io

## This requires the fabric module for python. Get it with
##      `pip install fabric`
## If you don't have pip, get that with
##      `easy_install pip`

## This script also assumes you have your package under source control with
## Mercurial. This is a good idea anyway. 

## If you only have your package under Mercurial locally (i.e. there is no remote
## repository you push it to), be sure to comment out the `local("hg push")` line
## from the tag() function below.

## For more details on fabric, see http://docs.fabfile.org/

from os.path import join, expanduser, abspath
from fabric.api import run, cd, env, sudo, local, put
import subprocess
import time
import re

## pkg is the name of the R package to build.
## pkgdir is the name of the directory within the repo that contains the package
##  (usually 'pkg')
## R_repo indicates the directory on the package repository server where the files should be 
##  put to. 
##  R_repo can also be passed in from the CLI.
## sourceonly tells the function whether to build only the .tar.gz or whether to 
##  also build the .tgz and .zip binaries.

pkg = 'rcrunch'
pkgdir = 'pkg'
R_repo = ''
sourceonly = False
testing = True
R_versions = [ '2.14', '2.15' ]

## These tell fabric where the R repository is and where the packages should be 
## uploaded. 
env.hosts = ['']
destination_base = '/var/www/html/R'

## Here's where you specify user profiles that you can call from the cli.
## (for ssh'ing to env.hosts)
users = {
    }

## These next dicts are not config settings.
destination_paths = {
                        'gz': 'src/contrib',
                        'tgz': 'bin/macosx/leopard/contrib/%s',
                        'zip': 'bin/windows/contrib/%s'
                    }
                            
PACKAGES_args = {
        'verbose': True,
        'unpacked': False,
        'subdirs': False,
        'latestOnly': True, 
        'addFiles': True
    }
    
PACKAGES_type = {
                    'gz': 'source',
                    'tgz': 'mac.binary',
                    'zip': 'win.binary'
                }

def ping(ip):
    """Pings subnet"""
    ret = subprocess.call("ping -c 1 %s" % ip,
                    shell=True,
                    stdout=open('/dev/null', 'w'),
                    stderr=subprocess.STDOUT) == 0
    return ret

def getLastVer():
    proc = subprocess.Popen(['hg', 'tags'], stdout=subprocess.PIPE)
    tip = next(proc.stdout)
    assert tip.startswith('tip'), "You're already on a tagged version."
    try:
        last_ver = next(proc.stdout)
        last_ver = last_ver.rsplit(None, 1)[0]
    except StopIteration:
        last_ver = '(none tagged)'
    proc.wait()
    return(last_ver)

def isATag(ver):
    """Returns boolean for whether `ver` is a valid tag name in the hg repo"""
    tags = local("hg tags", capture=True).split('\n')
    tags = ( t.rsplit(None, 1)[0] for t in tags )
    return ver in tags

def document():
    local("R --slave -e 'library(devtools); document(\"%s\")'" % pkgdir)
    local("git add %s/man/*.Rd" % pkgdir)

def _tag(ver):
    """update DESCRIPTION with the new version and today's date"""
    today = time.strftime("%Y-%m-%d", time.localtime())
    dfilepath = abspath(join(pkgdir, 'DESCRIPTION'))
    with open(dfilepath, 'r') as dfile:
        des = dfile.readlines()
        for i, line in enumerate(des):
            if line.split(':')[0]=="Version":
                des[i] = 'Version: ' + ver + "\n"
            elif line.split(':')[0]=="Date":
                des[i] = 'Date: ' + today + "\n"
    with open(dfilepath, 'w') as dfile:
        dfile.writelines(des)
    local("hg ci -m 'Releasing %s version %s'" % (pkg, ver))
    local("hg tag %s" % ver)
    local("hg push")

def test(): 
    """Run R package tests"""
    local("R CMD INSTALL %s" % pkgdir)
    return local("R --slave -e 'library(testthat); test_package(\"%s\")'" % pkg).succeeded

## The next functions facilitate making R function calls
fcall = re.compile("^[a-z]+\(.*\)$")

def is_function_call(x):
    return fcall.match(x) is not None

def serialize(x, asString=False):
    if isinstance(x, (list, tuple, set, frozenset)) and len(x)>0:
        out = "c(%s)" % (', '.join(serialize(i, asString) for i in x))
    elif isinstance(x, dict):
        out = Rcall('list', **x)
    else:
        out = str(x)
        if isinstance(x, bool):
            out = out.upper()
        if asString or (isinstance(x, basestring) and not is_function_call(x)):
            out = '"'+out+'"'

    return out

def Rcall(function_name, *ar, **kw):
    out = []
    for i in ar:
        out.extend([serialize(i)])
    for i in kw:
        out.extend(['%s=%s' % (i, serialize(kw[i]))])
    return '%s(%s)' % (function_name, ', '.join(out))

####################
## Here's the fab stuff

def tag(tests=True):
    """Update the package DESCRIPTION, then tag a release in hg"""
    last_ver = getLastVer()
    ver = raw_input("Last was {last_ver}; What version? ".format(**vars()))
    if not isATag(ver):
        document()
        if tests:
            passed = test()
            assert passed, "Tests failed! Fix it!"
        _tag(ver)
    return ver

def build(ver):
    """Select a tag (version) in the hg repo to build, then build it"""
    print 'Building %s version %s' % (pkg, ver)
    cbranch = local('hg branch', capture=True)
    local('hg up ' + ver)
    
    args = '--args pkg="%s" path="%s"' % (pkg, abspath(pkgdir))
    files = []
    local("R CMD BUILD %s" % pkgdir)
    files.append(pkg + '_' + ver + '.tar.gz')
    if not sourceonly:
        local("R CMD INSTALL --build %s" % pkgdir)
        files.append(pkg + '_' + ver + '.tgz')
        f3 = local("R --slave --file=" + abspath("build.R") + " '" + args + "'", capture=True).split('\n')
        files.append(f3.pop())
    for i in files:
        local("chmod 644 " + i)
    print 'Returning to %s branch' % cbranch
    local('hg up %s' % cbranch)
    return files

def copy_to_repo(user, files, R_version=R_versions, repodir=R_repo):
    """Upload the built packages to the R package repository"""
    if user in users:
        user = users[user]['username']
    env.user = user 
    for i in files:
        dest = join(destination_base, repodir,
            destination_paths[i.split('.').pop()])
        if '%s' in dest:
            for j in R_version:
                de = dest % j
                destfilename = join(de, i.split('/').pop())
                put(i, de, mode=644)
                run("chmod 644 " + destfilename)
        else:
            destfilename = join(dest, i.split('/').pop())
            put(i, dest, mode=644)
            run("chmod 644 " + destfilename)
    
def update_pkg_repo(user, R_version=R_versions, repodir=R_repo):
    """Update the PACKAGES index on the remote package repository"""
    if user in users:
        user = users[user]['username']
    env.user = user
    with cd(join(destination_base, repodir)):
        for i in PACKAGES_type:
            PACKAGES_args['type'] = PACKAGES_type[i]
            if '%s' in destination_paths[i]:
                if not sourceonly:
                    for j in R_version:
                        PACKAGES_args['dir'] = destination_paths[i] % j
                        run("R -e 'library(tools); %s'" % Rcall("write_PACKAGES",
                            **PACKAGES_args))
            else:
                PACKAGES_args['dir'] = destination_paths[i]
                run("R -e 'library(tools); %s'" % Rcall("write_PACKAGES",
                    **PACKAGES_args))

def release(user, repodir=R_repo, tests=testing):
    """Wrap all of that and make a release"""
    if ping(env.hosts[0]):
        ver = tag(tests)
        files = build(ver)
        copy_to_repo(user, files, repodir=repodir)
        update_pkg_repo(user, repodir=repodir)
    else:
        print "Unable to reach %s. Maybe you need to connect the VPN?" % env.hosts[0]
