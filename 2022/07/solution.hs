import Data.List
import Data.List.Split (splitOn)


data Node = Directory String [Node] | File String Int deriving (Show)

parseCmd :: [String] -> Node -> ([String], Node)
parseCmd [] dir = ([], dir)
parseCmd (cmd : cmds) dir = res where
    (Directory name files) = dir
    res
        | cmd == "$ cd .."        = (cmds, Directory name files)
        | "dir"  `isPrefixOf` cmd = parseCmd cmds (Directory name files)
        | "$ ls" `isPrefixOf` cmd = parseCmd cmds (Directory name files)
        | "$ cd" `isPrefixOf` cmd = parseCmd updated_cmd (Directory name (sub_folder : files))
        | otherwise               = parseCmd cmds (Directory name (file : files))

    cmd_split = words cmd
    [size, fname] = words cmd
    dir_name = cmd_split !! 2
    file = File fname (read size)
    (updated_cmd, sub_folder) = parseCmd cmds (Directory dir_name [])

sizeOfNode node = case node of
  (File _ size)     -> size
  (Directory _ nl)  -> sum (map sizeOfNode nl)

part1 node = case node of
  File _ _ -> 0
  Directory _ nl -> sum (map part1 nl) + node_contribution where
    node_size = sizeOfNode node
    node_contribution = if node_size > 100000 then 0 else node_size

part2 (File _ _) delete_size total_size = total_size
part2 dir delete_size total_size = res where 
  (Directory _ sub_dirs) = dir
  node_size = sizeOfNode dir
  cand_size = if node_size > delete_size then node_size else total_size
  cands = map (\s -> part2 s delete_size total_size) sub_dirs
  res = minimum (cand_size:cands)

main = do
  input <- readFile "input.dat"
  let input_lines = lines input
  let (_, root) = parseCmd (tail input_lines) (Directory "/" [])
  let root_size = sizeOfNode root

  print (part1 root)

  let total_size = 70000000
  let update_size = 30000000 
  let delete_size = update_size - (total_size - root_size)
  print (part2 root delete_size total_size)
