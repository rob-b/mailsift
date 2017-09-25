# mailsift

Mailsift works with sendgrid's [Inbound Email Parse
Webhook](https://sendgrid.com/docs/API_Reference/Webhooks/inbound_email.html)
to dump emails to a database. We use it at work with a BCC email address to
keep logs of our testing emails.

## Installation
Clone this repo and `stack build --install-ghc`
