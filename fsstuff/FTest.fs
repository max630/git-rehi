module rebase2.FTest

let gitResolveHashes refs =
    for ref in refs do
        IOUtils.verifyCmdArg ref