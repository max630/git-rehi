module rebase2.FTest

let intersperse sep sq =
    let fun1 st it = match st with
                     | (true, acc) -> (true, acc ^ sep ^ it)
                     | (false, acc) -> (false, acc ^ it)
    let (_, res) = Seq.fold fun1 (false, "") sq
    res

let resolveHashes refs =
    let refs_list = Seq.toList refs
    for ref in refs_list do
        IOUtils.verifyCmdArg ref
    let refs_str = intersperse " " refs_list
    (* newlines should be stripped by EnumPopen implementation *)
    let res = Seq.toList (IOUtils.EnumPopen("git", "rev-parse " ^ refs_str))
    if (List.length res <> List.length refs_list) then
        raise (System.Exception "Hash number does not match")
    res