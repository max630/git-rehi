namespace rebase2

let gitResolveHashes refs =
    for ref in refs do
        IOUtils.verifyCmdArg ref