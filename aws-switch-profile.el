;;; aws-switch-profile.el --- Switch AWS IAM roles and set environment variables -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides functionality to switch AWS IAM roles using the AWS CLI
;; and set environment variables in Emacs accordingly.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'eieio)
(require 'parse-time)

(defgroup aws-switch-profile nil
  "Manage AWS IAM roles and environment variables in Emacs."
  :group 'applications
  :prefix "aws-switch-profile-")

(defcustom aws-switch-profile-aws-command-path "aws"
  "Path to the AWS CLI executable."
  :type 'string
  :group 'aws-switch-profile)

(defcustom aws-switch-profile-aws-config-file "~/.aws/config"
  "Path to the AWS configuration file."
  :type 'file
  :group 'aws-switch-profile)

(defcustom aws-switch-profile-aws-role-session-name "EmacsSession"
  "Default AWS IAM role session name."
  :type 'string
  :group 'aws-switch-profile)

(defun aws-switch-profile--parse-config ()
  "Parse the AWS config file and extract profiles information."
  (when (file-readable-p aws-switch-profile-aws-config-file)
    (with-temp-buffer
      (insert-file-contents aws-switch-profile-aws-config-file)
      (let (profiles)
        (while (re-search-forward "\\[profile \\(.*?\\)\\]" nil t)
          (let ((current-profile (match-string 1))
                (config-data (make-hash-table :test 'equal)))
            (forward-line)
            (while (and (not (eobp)) (not (looking-at "\\[.*\\]")))
              (when-let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (string-match "\\([^=]+\\)\\s-*=\\s-*\\(.*\\)" line)
                  (let ((key (match-string-no-properties 1 line))
                        (value (match-string-no-properties 2 line)))
                    (puthash (string-trim key) (string-trim value) config-data))))
              (forward-line))
            (push (cons current-profile config-data) profiles)))
        (nreverse profiles)))))

(defun aws-switch-profile--filter-profiles (profiles)
  "Filter PROFILES to include only those with a `source_profile' key."
  (cl-remove-if-not (lambda (profile)
                      (gethash "source_profile" (cdr profile)))
                    profiles))

(defclass aws-switch-profile-session ()
  ((access-key-id
    :initarg :access-key-id
    :initform ""
    :type string
    :documentation "access key id")
   (secret-access-key
    :initarg :secret-access-key
    :initform ""
    :type string
    :documentation "secret key id")
   (session-token
    :initarg :session-token
    :initform ""
    :type string
    :documentation "session token")
   (expiration
    :initarg :expiration
    :initform nil
    :type list
    :documentation "expiration")))

(cl-defmethod aws-switch-profile-session-setenv ((session aws-switch-profile-session))
  "Set the current environment's AWS credentials based on SESSION object."
  (setenv "AWS_ACCESS_KEY_ID" (oref session access-key-id))
  (setenv "AWS_SECRET_ACCESS_KEY" (oref session secret-access-key))
  (setenv "AWS_SESSION_TOKEN" (oref session session-token)))

(defvar aws-switch-profile--session-credentials-cache (make-hash-table :test 'equal)
  "Cache for storing AWS session credentials.")

(defun aws-switch-profile--get-session-credentials (role-arn)
  "Get the AWS session credentials from the cache if it exists and is not expired.
ROLE-ARN is the role Amazon Resource Name.

If the credentials are expired or don't exist, return nil."
  (let ((session-object (gethash role-arn aws-switch-profile--session-credentials-cache)))
    (when session-object
      (let ((expiration-time (oref session-object expiration)))
        ;; If expiration-time is not set (nil) or is in the future, return the credentials.
        ;; Convert the expiration string to time and compare it to the current time.
        (if (or (null expiration-time)
                (time-less-p (current-time) expiration-time)
                session-object))))))

(defun aws-switch-profile--set-env-vars (role-arn session-info)
  "Set AWS environment variables based on SESSION-INFO."
  (pcase-let ((`((Credentials . ((AccessKeyId . ,access-key-id)
                                 (SecretAccessKey . ,secret-access-key)
                                 (SessionToken . ,session-token)
                                 (Expiration . ,expiration))))
               session-info))
    (let ((session (aws-switch-profile-session
                    :access-key-id access-key-id
                    :secret-access-key secret-access-key
                    :session-token session-token
                    :expiration (parse-iso8601-time-string expiration))))
      (aws-switch-profile-session-setenv session)
      (puthash role-arn session aws-switch-profile--session-credentials-cache))))

(defun aws-switch-profile--get-session-info (source-profile role-arn role-session-name duration-seconds mfa-serial mfa-code)
  "Retrieve session information for a given AWS IAM role."
  (let ((cmd (format "%s sts assume-role --profile %s --role-arn %s --role-session-name %s --duration-seconds %d --output json"
                     aws-switch-profile-aws-command-path source-profile role-arn role-session-name duration-seconds))
        (mfa-params (when mfa-serial (format "--serial-number %s --token-code %s" mfa-serial mfa-code))))
    (json-read-from-string (shell-command-to-string (if mfa-params (concat cmd " " mfa-params) cmd)))))

;;;###autoload
(defun aws-switch-profile ()
  "Switch AWS IAM role and set environment variables."
  (interactive)
  (let* ((profiles (aws-switch-profile--filter-profiles (aws-switch-profile--parse-config)))
         (profile-name (completing-read "Select an AWS profile: " (mapcar #'car profiles)))
         (profile (cdr (assoc profile-name profiles))))
    (pcase-let ((`(,role-arn ,source-profile ,mfa-serial)
                 (mapcar (lambda (key) (gethash key profile))
                         '("role_arn" "source_profile" "mfa_serial"))))
      (when role-arn
        (if-let (session (aws-switch-profile--get-session-credentials role-arn))
            (aws-switch-profile-session-setenv session)
          (let ((mfa-code (when mfa-serial (read-string "Enter MFA code: ")))
                (role-session-name (or (gethash "role_session_name" profile) aws-switch-profile-aws-role-session-name))
                (duration-seconds 3600))
            (aws-switch-profile--set-env-vars role-arn
                                              (aws-switch-profile--get-session-info source-profile role-arn role-session-name duration-seconds mfa-serial mfa-code))))
        (message "Switched to AWS profile: %s" profile-name)))))

;;;###autoload
(defun aws-switch-profile-clear-aws-session ()
  "Clear AWS session environment variables to revert to the default credentials."
  (interactive)
  (setenv "AWS_PROFILE" nil)
  (setenv "AWS_ACCESS_KEY_ID" nil)
  (setenv "AWS_SECRET_ACCESS_KEY" nil)
  (setenv "AWS_SESSION_TOKEN" nil)
  (setq aws-switch-profile--session-credentials-cache (make-hash-table :test 'equal))
  (message "AWS session variables cleared, reverted to default credentials."))

(provide 'aws-switch-profile)

;;; aws-switch-profile.el ends here
