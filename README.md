`nix-simple-ci`
---

This is a very simple CI server for Nix projects on GitHub. It listens
for the GitHub `push` webhook, builds the repo's `ci.nix` file in a
Nix sandbox, and adds a status indicator to the commit with the GitHub
API. This is particularly nice when the CI server is also a Nix binary
chache, since these builds will automatically be cached for everyone
else on your team.

Usage
---

- Add a `ci.nix` file to your repo, such that `nix-build ci.nix`
  performs the CI build.
- Generate a random key for GitHub Webhooks and
  [add it to the Webhook UI for your repo](https://developer.github.com/webhooks/securing/). This
  is used by your server to establish trust for incoming Webhook
  payloads. For example:

  ```bash
  $ openssl rand -base64 32
  ```
- Generate an OAuth2 token with access to the `repo:status` scope so
  that `nix-simple-ci` can update commit CI status on your behalf. The
  easiest way to do this is with
  [personal access tokens](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/).
- Launch the server.

  ```bash
  $ nix-simple-ci --secret <webhook-shared-secret> --oauth <github-api-token> [--port <port-number>]
  ```

TODO
---

- It'd be nice if the log from the `nix-build` were put in the
  `target_url` link of the status indicator. This would require either
  persisting logs on the server for later viewing, or merely posting
  them to a site like pastebin.
- You can't use the statuses API to add a status to a commit on
  someone's fork of your repo, meaning you can't get CI status for PRs
  from forks. It'd be great if there were a way to do this. Maybe just
  build PRs and leave something like a comment?
- Document launching the server in a systemd unit using NixOS modules.
- Support multiple Webhook tokens via the `servant-github-webhook`
  library, and support multiple access tokens.
