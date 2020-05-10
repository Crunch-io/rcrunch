keyring::key_delete("crunch")
Sys.unsetenv("R_CRUNCH_EMAIL")
Sys.unsetenv("R_CRUNCH_PW")


## ==== No saved passwords ===

#Username passed via text
login("test@crunch.io")

#Username saved via system environmeent
Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
login()
Sys.unsetenv("R_CRUNCH_EMAIL")



## ==== Single saved password ====

keyring::key_set_with_value("crunch", username = "test@crunch.io", password = "abc123")

#Single Keyring password, username specified via login, saved password matches specified username
login("test@crunch.io")

#Single keyring password, username specified via login, saved password for different username
login("test@yougov.com")

#Single keyring password, username saved in keyring
login()

#Single keyring password, username saved in system environment, saved password matches specified email
Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
login()
Sys.unsetenv("R_CRUNCH_EMAIL")

#Single keyring password, username saved in system environment, saved password doesn't match specified email
Sys.setenv(R_CRUNCH_EMAIL = "test@yougov.com")
login()
Sys.unsetenv("R_CRUNCH_EMAIL")

#Single keyring password, no saved username
keyring::key_delete("crunch")
keyring::key_set_with_value("crunch", password = "abc123")
login()



## ==== Multiple saved passwords ====

#Multiple keyring passwords, logging in with username corresponding to saved password
keyring::key_set_with_value("crunch", username = "user@gmail.com", password = "foobar")
keyring::key_set_with_value("crunch", username = "test@crunch.io", password = "abc123")
login("user@gmail.com")
login("test@crunch.io")

#Multiple keyring passwords, logging in with username NOT corresponding to saved password
login("test@yougov.com")

#Multiple keyring passwords, attempting to log in without specified username
login()

#Username set via environment variable, 2+ saved passwords in keyring, usernane via environemnt variable matching keyring
Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
login()
Sys.unsetenv("R_CRUNCH_EMAIL")

#Username set via environment variable, 2+ saved passwords in keyring, username via environemnt variable NOT matching keyring
Sys.setenv(R_CRUNCH_EMAIL = "test@yougov.com")
login()
Sys.unsetenv("R_CRUNCH_EMAIL")



## ==== Password saved in global environment, username in keyring ====
# This is a rare scenario, but still worth checking

#Mutliple usernames saved in keyring
Sys.setenv(R_CRUNCH_PW = "abc123")
login()
Sys.unsetenv("R_CRUNCH_PW")

#Single username saved in keyring
Sys.setenv(R_CRUNCH_PW = "abc123")
keyring::key_delete("crunch", username = "user@gmail.com")
keyring::key_delete("crunch", username = "")
login()
Sys.unsetenv("R_CRUNCH_PW")



## ==== Old use case - password and username via global environment ====

# Username and password both saved
Sys.setenv(R_CRUNCH_PW = "abc123")
Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
login()

# Username only saved
Sys.unsetenv("R_CRUNCH_PW")
login()
login("test@yougov.com")

# Password only saved
Sys.unsetenv("R_CRUNCH_EMAIL")
Sys.setenv(R_CRUNCH_PW = "abc123")
login()
login("test@crunch.io")






