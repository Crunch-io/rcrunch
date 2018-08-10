\newcommand{\mocksetup}{\dontshow{library(httptest); .mockPaths(system.file(package="crunch")); use_mock_api()}}
\newcommand{\mockteardown}{\dontshow{stop_mocking()}}
\newcommand{\mocked}{\mocksetup #1 \mockteardown}
