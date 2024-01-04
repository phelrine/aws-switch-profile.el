# aws-switch-profile.el

## Overview

`aws-switch-profile.el` is an Emacs Lisp package designed to facilitate switching between AWS IAM roles using the AWS CLI directly from Emacs. It simplifies the process of managing AWS environment variables based on the selected IAM role, enhancing the efficiency of AWS-related workflows within Emacs.

## Features

- Easy switching between AWS IAM roles.
- Automatic setting of AWS environment variables in Emacs.
- Support for MFA-enabled AWS accounts.
- Customizable AWS CLI path and session duration.

## Installation

To install `aws-switch-profile.el`, clone this repository and add the following code to your Emacs configuration:

```elisp
(add-to-list 'load-path "path/to/aws-switch-profile")
(require 'aws-switch-profile)
```

Replace "path/to/aws-switch-profile" with the actual path to where you cloned the repository.

## Usage

To switch AWS profiles, simply run the command M-x aws-switch-profile. You'll be prompted to select an AWS profile. If MFA is required, you will also be prompted to enter the MFA code.

## Customization

`aws-switch-profile.el` offers several customization options to adapt the package to your AWS setup and preferences. You can configure these settings in your Emacs initialization file. Here are the customizable variables and their default settings:

- `aws-switch-profile-aws-command-path`: Sets the path to the AWS CLI executable. Default is `"aws"`, but this should be adjusted based on your system's installation path.

    Example:
    ```elisp
    (setq aws-switch-profile-aws-command-path "/usr/local/bin/aws")
    ```

- `aws-switch-profile-aws-config-file`: Specifies the path to your AWS configuration file. The default is `"~/.aws/config"`.

    Example:
    ```elisp
    (setq aws-switch-profile-aws-config-file "~/.aws/config")
    ```

- `aws-switch-profile-aws-role-session-name`: Sets the default session name for AWS IAM roles. Default is `"EmacsSession"`. Note that if `role_session_name` is specified in the AWS config file for a particular profile, that name takes precedence over this setting.

    Example:
    ```elisp
    (setq aws-switch-profile-aws-role-session-name "MyCustomSessionName")
    ```

These settings allow you to customize the package behavior to suit your AWS environment and usage patterns. Adjust these variables as necessary for your configuration.
