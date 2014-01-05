module rebase2.FTest

let resolveHashes refs =
    for ref in refs do
        IOUtils.verifyCmdArg ref