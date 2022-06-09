![Logo](https://github.com/boonlogic/amber-go-sdk/blob/master/docs/BoonLogic.png?raw=true)

# Boon Amber R SDK

An R SDK for Boon Amber sensor analytics

- __Website__: [boonlogic.com](https://boonlogic.com)
- __Documentation__: [Boon Docs Main Page](https://docs.boonlogic.com)

## Installation

```R
install.packages("BoonAmber_1.0.0.tar.gz",  repos = NULL, type="source")

library(BoonAmber)
```

## Credentials setup

You'll need the `devtools` package in order to build the API.
Make sure you have a proper CRAN repository from which you can download packages.


Note: An account in the Boon Amber cloud must be obtained from Boon Logic to use the Amber SDK.

The username and password should be placed in a file named _~/.Amber.license_ whose contents are the following:

```json
{
    "default": {
        "username": "AMBER-ACCOUNT-USERNAME",
        "password": "AMBER-ACCOUNT-PASSWORD",
        "server": "https://amber.boonlogic.com/v1"
    }
}
```

The _~/.Amber.license_ file will be consulted by the Amber SDK to find and authenticate your account credentials with the Amber server. Credentials may optionally be provided instead via the environment variables `AMBER_USERNAME` and `AMBER_PASSWORD`.




