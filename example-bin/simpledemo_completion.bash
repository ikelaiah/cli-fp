#!/bin/bash
declare -A tree
tree["__root__|subcommands"]="greet"
tree["__root__|params"]="--help --help-complete --version --completion-file --completion-file-pwsh -h -v"
tree["greet|subcommands"]=""
tree["greet|params"]="--name -n --count -c --verbose -d --mode -m --help -h --version -v"

_myapp_completions()
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
  out=$("./SimpleDemo.exe" __complete "${args[@]}")
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
complete -F _myapp_completions SimpleDemo.exe
complete -F _myapp_completions ./SimpleDemo.exe
