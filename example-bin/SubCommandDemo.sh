#!/bin/bash
_repomanager_completions()
{
  local cur prev words cword
  _init_completion -n : || return
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  words=("${COMP_WORDS[@]}")
  cword=$COMP_CWORD
  local opts="repo --help --help-complete --version --completion-file"
  case "${words[1]}" in
    "repo" )
      opts="init clone remote  --help --help-complete --version --completion-file"
      ;;
    "repo init" )
      opts="--path -p --bare -b --help --help-complete --version --completion-file"
      ;;
    "repo clone" )
      opts="--url -u --path -p --branch -b --depth -d --help --help-complete --version --completion-file"
      ;;
    "repo remote" )
      opts="add remove  --help --help-complete --version --completion-file"
      ;;
    "repo remote add" )
      opts="--name -n --url -u --help --help-complete --version --completion-file"
      ;;
    "repo remote remove" )
      opts="--name -n --help --help-complete --version --completion-file"
      ;;
    *)
      opts="repo --help --help-complete --version --completion-file"
      ;;
  esac
  COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
  return 0
}
complete -F _repomanager_completions SubCommandDemo.exe
