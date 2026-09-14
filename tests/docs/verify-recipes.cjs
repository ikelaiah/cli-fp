// Compile the current Markdown, not a second copy of its Pascal examples.
// Requires Node.js and FPC on PATH; uses only Node's standard library.
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');
const assert = require('node:assert/strict');
const root = path.resolve(__dirname, '../..');
const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'cli-fp-doc-recipes-'));
const windows = process.platform === 'win32';
let programs = 0, invocations = 0;
const read = file => fs.readFileSync(path.join(root, file), 'utf8').replace(/\r\n/g, '\n');
const blocks = (file, language) => [...read(file).matchAll(/^```([^\n]*)\n([\s\S]*?)^```/gm)]
  .filter(match => match[1] === language).map(match => match[2]);
function run(command, args, cwd = root, expected = 0) {
  const result = spawnSync(command, args, { cwd, encoding: 'utf8', timeout: 120000 });
  assert.ifError(result.error);
  assert.equal(result.status, expected,
    `${command} ${args.join(' ')}\n${result.stdout}\n${result.stderr}`);
  invocations++;
  return result.stdout;
}
const units = 'CLI.Interfaces, CLI.Application, CLI.Command, CLI.Console, CLI.Progress, SysUtils';
const withoutUses = source => source.replace(/uses\s+[\s\S]*?;/, '');
function program(pattern, setup) {
  return `program Recipe;\n{$mode objfpc}{$H+}{$J-}\nuses ${units};\n${withoutUses(pattern)}\n${setup}`;
}
function compile(source) {
  const dir = path.join(temp, `program-${++programs}`);
  fs.mkdirSync(path.join(dir, 'units'), { recursive: true });
  const file = path.join(dir, 'Recipe.lpr');
  fs.writeFileSync(file, source);
  run('fpc', [`-Fu${path.join(root, 'src')}`, `-FE${dir}`, `-FU${path.join(dir, 'units')}`, file]);
  return path.join(dir, `Recipe${windows ? '.exe' : ''}`);
}
function expect(exe, args, text, status = 0) {
  const output = run(exe, args, temp, status);
  assert.ok(output.includes(text), `Expected ${JSON.stringify(text)} in ${output}`);
}
function methodProgram(className, method, setupOptions = '') {
  return program(`type ${className} = class(TBaseCommand)
    public function Execute: Integer; override; end;\n${method}`,
  `var App: ICLIApplication; Cmd: ${className}; begin
    Cmd := ${className}.Create('', 'Recipe'); ${setupOptions}
    App := CreateCLIApplication('recipe', '1.0.0', Cmd); Halt(App.Execute); end.`);
}
function checkLinks() {
  const files = ['README.md', ...fs.readdirSync(path.join(root, 'docs'))
    .filter(name => name.endsWith('.md')).map(name => `docs/${name}`)];
  for (const file of files) {
    for (const match of read(file).matchAll(/\]\(([^)]+)\)/g)) {
      const href = match[1];
      if (/^(https?:|mailto:)/.test(href)) continue;
      const [target] = href.split('#');
      const resolved = path.resolve(root, path.dirname(file), decodeURIComponent(target || path.basename(file)));
      assert.ok(fs.existsSync(resolved), `${file}: broken link ${href}`);
    }
  }
  console.log('Current Markdown local links resolve');
}
function shellRecipe(file, index, expectedText) {
  // The checkout/cd preamble is fulfilled by this isolated local source copy.
  const language = windows ? 'powershell' : 'bash';
  const code = blocks(file, language)[index];
  assert.ok(code, `Missing ${language} block ${index} in ${file}`);
  const cwd = path.join(temp, `shell-${invocations}`);
  fs.mkdirSync(cwd);
  for (const dir of ['src', 'examples', 'tools'])
    fs.cpSync(path.join(root, dir), path.join(cwd, dir), { recursive: true });
  const lines = code.split('\n').filter(line => line.trim() &&
    !line.startsWith('git clone ') && line !== 'cd cli-fp' && line !== 'Set-Location cli-fp');
  const script = windows ? "$ErrorActionPreference = 'Stop'\n" + lines.map(line =>
    /^(fpc |\.\\)/.test(line) ? `${line}\nif ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }` : line).join('\n') :
    'set -euo pipefail\n' + lines.join('\n');
  const output = windows ? run('powershell', ['-NoProfile', '-Command', script], cwd) :
    run('bash', ['-c', script], cwd);
  assert.ok(output.includes(expectedText), `${file}: expected ${expectedText}\n${output}`);
}
try {
  checkLinks();
  for (const file of ['README.md', 'docs/getting-started.md']) {
    const source = blocks(file, 'pascal')[0];
    assert.equal(source.replace(/\s+/g, ''),
      read('examples/QuickStartDemo/QuickStartDemo.lpr').replace(/\s+/g, ''));
    const exe = compile(source);
    expect(exe, [], 'Hello, World!');
    expect(exe, ['--name', 'Ada'], 'Hello, Ada!');
    expect(exe, ['--name', 'Alice', '-n', 'Bob'], 'Hello, Bob!');
    expect(exe, ['-n', 'Alice', '--NAME=Bob'], 'Hello, Bob!');
    expect(exe, ['--name=--version'], 'Hello, --version!');
    expect(exe, ['--name', 'Ada', '-V'], 'hello version 1.0.0');
    expect(exe, ['--help'], '--name');
    expect(exe, ['--unknown'], 'Unknown parameter', 1);
  }
  const how = blocks('docs/how-to.md', 'pascal');
  const greet = compile(program(how[0], how[1]));
  expect(greet, ['greet'], 'Hello, World!');
  expect(greet, ['greet', '--name', 'Ada'], 'Hello, Ada!');
  const registrations = how.filter(block => /^Greet\.Add/.test(block));
  assert.equal(registrations.length, 3);
  const setup = how[1].replace(/^  Greet\.AddStringParameter.*$/m, registrations.join('\n'));
  const combined = compile(program(how[0], setup));
  const combinedArgs = ['greet', '--count', '2', '--file', 'input.txt', '--path', '.', '--url',
    'https://example.com/project.git', '--api-key', 'recipe-secret', '--name', 'Ada'];
  expect(combined, combinedArgs, 'Hello, Ada!');
  expect(combined, ['greet'], 'Required parameter', 1);
  const lookupMethod = how.find(block => block.includes('RawCount: string;'));
  const lookup = compile(program(how[0].replace(/function TGreetCommand\.Execute: Integer;[\s\S]*$/, lookupMethod), setup));
  expect(lookup, [...combinedArgs, '--verbose'], 'Verbose mode');
  expect(lookup, [...combinedArgs, '--verbose=false'], 'Count: 2');
  const check = how.find(block => block.includes('function TCheckCommand.Execute'));
  const checkExe = compile(methodProgram('TCheckCommand', check,
    "Cmd.AddStringParameter('-f', '--file', 'Input file');"));
  expect(checkExe, [], '', 1);
  expect(checkExe, ['--file', 'input.txt'], 'Checking input.txt');
  const commandBlocks = blocks('docs/commands.md', 'pascal');
  const howRoot = compile(program(commandBlocks[0], how[2]));
  expect(howRoot, ['--verbose=false'], 'Running the default action');
  const howNested = compile(program(commandBlocks[4], how[3]));
  expect(howNested, ['repo', 'clone', '--url', 'https://example.com/project.git'], 'Cloning a repository');
  for (let i = 0; i < commandBlocks.length; i += 2) {
    const exe = compile(program(commandBlocks[i], commandBlocks[i + 1]));
    const args = [[], ['greet', '--name', 'Ada'], ['repo', 'clone', '--url', 'https://example.com/project.git']][i / 2];
    expect(exe, args, ['Running the default action', 'Hello from greet', 'Cloning a repository'][i / 2]);
    expect(exe, [...args, '-v'], 'version 1.0.0');
  }
  const options = blocks('docs/options.md', 'pascal');
  const optionRows = [...read('docs/options.md').matchAll(/`(Cmd\.Add[^`]+)`/g)]
    .map(match => match[1].replace(/\\\|/g, '|') + ';');
  optionRows.push(options.find(block => block.startsWith('Cmd.AddEnumParameter')));
  const optionSetup = `var App: ICLIApplication; Cmd: TOptionsCommand; begin
    Cmd := TOptionsCommand.Create('configure', 'Configure'); ${optionRows.join('\n')}
    App := CreateCLIApplication('options', '1.0.0'); App.RegisterCommand(Cmd); Halt(App.Execute); end.`;
  const optionExe = compile(program(options[0].replace(/function TOptionsCommand\.Execute: Integer;[\s\S]*$/,
    options.find(block => block.includes('RawCount: string;'))), optionSetup));
  expect(optionExe, ['configure', '--count', '2', '--path', '.', '--url', 'https://example.com', '--api-key', 'secret'], 'Count: 2');
  for (const file of ['docs/how-to.md', 'docs/terminal.md']) {
    for (const block of blocks(file, 'pascal')) {
      if (block.includes('function TDownloadCommand.Execute')) {
        const exe = compile(methodProgram('TDownloadCommand', block));
        expect(exe, [], 'Downloading');
      } else if (block.includes('function TBatchCommand.Execute')) {
        const exe = compile(methodProgram('TBatchCommand', block));
        expect(exe, [], 'Processed 3 of 3');
      } else if (block.startsWith('TConsole.WriteLn')) {
        const exe = compile(methodProgram('TColourCommand',
          `function TColourCommand.Execute: Integer; begin ${block} Result := 0; end;`));
        expect(exe, [], file.includes('terminal') ? 'Done' : 'Created project');
      }
    }
  }
  // Run the actual documented shell commands from a clean repository root.
  shellRecipe('README.md', windows ? 0 : 1, 'Hello, Ada!');
  shellRecipe('docs/getting-started.md', 0, 'hello version 1.0.0');
  shellRecipe('docs/examples.md', 0, 'RootCommandDemo');
  shellRecipe('docs/codegen.md', 0, 'greet');
  shellRecipe('docs/how-to.md', 1, 'myapp');
  const generatorDir = path.join(temp, 'generator');
  fs.mkdirSync(path.join(generatorDir, 'units'), { recursive: true });
  run('fpc', [`-Fu${path.join(root, 'tools/cli-fp-gen/src')}`, `-FE${generatorDir}`,
    `-FU${path.join(generatorDir, 'units')}`, path.join(root, 'tools/cli-fp-gen/cli_fp_gen.lpr')]);
  const generator = path.join(generatorDir, `cli_fp_gen${windows ? '.exe' : ''}`);
  const project = path.join(generatorDir, 'myapp');
  run(generator, ['init', project, '--name', 'myapp', '--version', '9.2.1']);
  assert.equal(JSON.parse(fs.readFileSync(path.join(project, 'clifp.json'))).app.version, '9.2.1');
  // The minimal JSON recipe replaces the initialized specification.
  fs.writeFileSync(path.join(project, 'clifp.json'), blocks('docs/codegen.md', 'json')[0]);
  run(generator, ['generate', '--project', project]);
  const output = path.join(project, 'build');
  fs.mkdirSync(path.join(output, 'units'), { recursive: true });
  run('fpc', [`-Fu${path.join(root, 'src')}`, '-Fusrc', '-Fusrc/generated', '-Fusrc/commands',
    `-FE${output}`, `-FU${path.join(output, 'units')}`, 'src/Hello.lpr'], project);
  const hello = path.join(output, `Hello${windows ? '.exe' : ''}`);
  expect(hello, ['--version'], 'hello version 1.0.0');
  expect(hello, ['--name', 'Ada'], 'TODO: Implement the root command');
  console.log(`Markdown recipes passed: ${programs} programs; ${invocations} compiler/process invocations (${process.platform})`);
} finally {
  fs.rmSync(temp, { recursive: true, force: true });
}
