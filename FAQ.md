# Frequently Asked Questions

1. [How do I fix invalid entitlements?  ](#how-do-i-fix-invalid-entitlements)

### How do I fix invalid entitlements?

You probably did not set `ios.bundleIdentifier` correctly in `default.nix`.
When this happens you'll see an error something like this:

```
2018-11-25 09:34:22.438 ios-deploy[58106:8521046] [ !! ] Error 0xe8008016: The executable was signed with invalid entitlements. AMDeviceSecureInstallApplication(0, device, url, options, install_callback, 0)
```

Fixing the value of `ios.bundleIdentifier` should fix the error.

