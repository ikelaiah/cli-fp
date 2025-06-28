#!/bin/bash
declare -A tree
tree["__root__|subcommands"]="repo"
tree["__root__|params"]="--help --help-complete --version --completion-file -h"
tree["repo|subcommands"]="init clone remote"
tree["repo|params"]="--help -h"
tree["repo init|subcommands"]=""
tree["repo init|params"]="--path -p --bare -b --help -h"
tree["repo clone|subcommands"]=""
tree["repo clone|params"]="--url -u --path -p --branch -b --depth -d --help -h"
tree["repo remote|subcommands"]="add remove"
tree["repo remote|params"]="--help -h"
tree["repo remote add|subcommands"]=""
tree["repo remote add|params"]="--name -n --url -u --help -h"
tree["repo remote remove|subcommands"]=""
tree["repo remote remove|params"]="--name -n --help -h"

_repomanager_completions()
{
  local cur words cword path subcmds params i
  # DEBUG: Print function call and COMP_WORDS
  # Uncomment for debugging:
  # echo "[DEBUG] Called: $FUNCNAME, COMP_WORDS=(\"${COMP_WORDS[@]}\") COMP_CWORD=$COMP_CWORD" >&2
  cur="${COMP_WORDS[COMP_CWORD]}"
  words=("${COMP_WORDS[@]}")
  cword=$COMP_CWORD
  # Determine path and index
  if [[ $cword -eq 1 ]]; then
    path="__root__"
    i=1
  else
    path="${words[1]}"
    i=2
    while [[ $i -le $cword ]]; do
      subcmds="${tree[$path|subcommands]}"
      found=0
      for sub in $subcmds; do
        if [[ "${words[$i]}" == "$sub" ]]; then
          path="$path $sub"
          found=1
          break
        fi
      done
      if [[ $found -eq 0 ]]; then break; fi
      ((i++))
    done
  fi
  subcmds="${tree[$path|subcommands]}"
  params="${tree[$path|params]}"
  # DEBUG: Print path, subcmds, params, i, cword
  # Uncomment for debugging:
  # echo "[DEBUG] path=[$path] subcmds=[$subcmds] params=[$params] i=$i cword=$cword cur=[$cur]" >&2
  if [[ -n "$subcmds" && $i -eq $cword ]]; then
    COMPREPLY=( $(compgen -W "$subcmds $params" -- "$cur") )
  else
    COMPREPLY=( $(compgen -W "$params" -- "$cur") )
  fi
  return 0
}
complete -F _repomanager_completions SubCommandDemo
complete -F _repomanager_completions ./SubCommandDemo
