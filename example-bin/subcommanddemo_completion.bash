#!/bin/bash
declare -A tree
tree["__root__|subcommands"]="repo"
tree["__root__|params"]="--help --help-complete --version --completion-file --completion-file-pwsh -h -v"
tree["repo|subcommands"]="init clone remote"
tree["repo|params"]="--help -h --version -v"
tree["repo init|subcommands"]=""
tree["repo init|params"]="--path -p --bare -b --help -h --version -v"
tree["repo clone|subcommands"]=""
tree["repo clone|params"]="--url -u --path -p --branch -b --depth -d --help -h --version -v"
tree["repo remote|subcommands"]="add remove"
tree["repo remote|params"]="--help -h --version -v"
tree["repo remote add|subcommands"]=""
tree["repo remote add|params"]="--name -n --url -u --help -h --version -v"
tree["repo remote remove|subcommands"]=""
tree["repo remote remove|params"]="--name -n --help -h --version -v"

_repomanager_completions()
{
  local cur words cword args out dir candidates
  cur="${COMP_WORDS[COMP_CWORD]}"
  words=("${COMP_WORDS[@]}")
  cword=$COMP_CWORD
  # Build args for __complete and call the application
  args=()
  for ((i=1;i<cword;i++)); do args+=("${words[i]}"); done
  # If cursor is after a space, append empty token to indicate new word
  if [[ "${COMP_LINE: -1}" == " " ]]; then
    args+=("")
  else
    args+=("${words[cword]}")
  fi
  out=$("./SubCommandDemo.exe" __complete "${args[@]}")
  # Last line is directive in form :<number>
  dir="$(printf "%s\n" "$out" | tail -n1)"
  if [[ $dir =~ ^:([0-9]+)$ ]]; then
    candidates="$(printf "%s\n" "$out" | sed '$d')"
    directive=${BASH_REMATCH[1]}
  else
    candidates="$out"
    directive=0
  fi
  # Populate COMPREPLY with matching candidates
  while IFS='' read -r comp; do
    [[ -z "$comp" ]] && continue
    COMPREPLY+=("$comp")
  done < <(compgen -W "$candidates" -- "$cur")
  return 0
}
complete -F _repomanager_completions SubCommandDemo.exe
complete -F _repomanager_completions ./SubCommandDemo.exe
