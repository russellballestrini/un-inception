// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software


// un.dart - Unsandbox CLI Client (Dart Implementation)
// Run: dart un.dart [options] <source_file>
// Compile: dart compile exe un.dart -o un
// Requires: UNSANDBOX_API_KEY environment variable

import 'dart:io';
import 'dart:convert';
import 'package:crypto/crypto.dart';

const String apiBase = 'https://api.unsandbox.com';
const String portalBase = 'https://unsandbox.com';
const String blue = '\x1B[34m';
const String red = '\x1B[31m';
const String green = '\x1B[32m';
const String yellow = '\x1B[33m';
const String reset = '\x1B[0m';

const Map<String, String> extMap = {
  '.py': 'python', '.js': 'javascript', '.ts': 'typescript',
  '.rb': 'ruby', '.php': 'php', '.pl': 'perl', '.lua': 'lua',
  '.sh': 'bash', '.go': 'go', '.rs': 'rust', '.c': 'c',
  '.cpp': 'cpp', '.cc': 'cpp', '.cxx': 'cpp',
  '.java': 'java', '.kt': 'kotlin', '.cs': 'csharp', '.fs': 'fsharp',
  '.hs': 'haskell', '.ml': 'ocaml', '.clj': 'clojure', '.scm': 'scheme',
  '.lisp': 'commonlisp', '.erl': 'erlang', '.ex': 'elixir', '.exs': 'elixir',
  '.jl': 'julia', '.r': 'r', '.R': 'r', '.cr': 'crystal',
  '.d': 'd', '.nim': 'nim', '.zig': 'zig', '.v': 'v',
  '.dart': 'dart', '.groovy': 'groovy', '.scala': 'scala',
  '.f90': 'fortran', '.f95': 'fortran', '.cob': 'cobol',
  '.pro': 'prolog', '.forth': 'forth', '.4th': 'forth',
  '.tcl': 'tcl', '.raku': 'raku', '.m': 'objc',
};

class Args {
  String? command;
  String? sourceFile;
  String? apiKey;
  String? network;
  int vcpu = 0;
  List<String> env = [];
  List<String> files = [];
  bool artifacts = false;
  String? outputDir;
  bool sessionList = false;
  String? sessionShell;
  String? sessionKill;
  bool serviceList = false;
  String? serviceName;
  String? servicePorts;
  String? serviceType;
  String? serviceBootstrap;
  String? serviceInfo;
  String? serviceLogs;
  String? serviceTail;
  String? serviceSleep;
  String? serviceWake;
  String? serviceDestroy;
  String? serviceExecute;
  String? serviceCommand;
  String? serviceDumpBootstrap;
  String? serviceDumpFile;
  bool keyExtend = false;
}

List<String?> getApiKeys(String? argsKey) {
  final publicKey = Platform.environment['UNSANDBOX_PUBLIC_KEY'];
  final secretKey = Platform.environment['UNSANDBOX_SECRET_KEY'];

  // Fall back to UNSANDBOX_API_KEY for backwards compatibility
  if (publicKey == null || publicKey.isEmpty || secretKey == null || secretKey.isEmpty) {
    final legacyKey = argsKey ?? Platform.environment['UNSANDBOX_API_KEY'];
    if (legacyKey == null || legacyKey.isEmpty) {
      stderr.writeln('${red}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set$reset');
      exit(1);
    }
    return [legacyKey, null];
  }

  return [publicKey, secretKey];
}

String detectLanguage(String filename) {
  final dotIndex = filename.lastIndexOf('.');
  if (dotIndex == -1) {
    throw Exception('Cannot detect language: no file extension');
  }
  final ext = filename.substring(dotIndex).toLowerCase();
  final lang = extMap[ext];
  if (lang == null) {
    throw Exception('Unsupported file extension: $ext');
  }
  return lang;
}

Future<Map<String, dynamic>> apiRequestCurl(String endpoint, String method, String? jsonData, String publicKey, String? secretKey, {String? baseUrl}) async {
  final base = baseUrl ?? apiBase;
  final tempFile = await File('${Directory.systemTemp.path}/un_request_${DateTime.now().millisecondsSinceEpoch}.json').create();

  try {
    final body = jsonData ?? '';
    if (jsonData != null) {
      await tempFile.writeAsString(jsonData);
    }

    final args = ['curl', '-s', '-X', method, '$base$endpoint',
                  '-H', 'Content-Type: application/json'];

    // Add HMAC authentication headers if secretKey is provided
    if (secretKey != null && secretKey.isNotEmpty) {
      final timestamp = (DateTime.now().millisecondsSinceEpoch ~/ 1000).toString();
      final message = '$timestamp:$method:$endpoint:$body';

      final key = utf8.encode(secretKey);
      final bytes = utf8.encode(message);
      final hmacSha256 = Hmac(sha256, key);
      final digest = hmacSha256.convert(bytes);
      final signature = digest.toString();

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      // Legacy API key authentication
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    if (jsonData != null) {
      args.addAll(['-d', '@${tempFile.path}']);
    }

    final result = await Process.run(args[0], args.sublist(1));

    if (result.exitCode != 0) {
      throw Exception('curl failed: ${result.stderr}');
    }

    final response = result.stdout as String;
    return jsonDecode(response) as Map<String, dynamic>;
  } finally {
    await tempFile.delete();
  }
}

Future<void> cmdExecute(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];
  final code = await File(args.sourceFile!).readAsString();
  final language = detectLanguage(args.sourceFile!);

  final payload = <String, dynamic>{
    'language': language,
    'code': code,
  };

  if (args.env.isNotEmpty) {
    final envVars = <String, String>{};
    for (final e in args.env) {
      final parts = e.split('=');
      if (parts.length == 2) {
        envVars[parts[0]] = parts[1];
      }
    }
    if (envVars.isNotEmpty) {
      payload['env'] = envVars;
    }
  }

  if (args.files.isNotEmpty) {
    final inputFiles = <Map<String, String>>[];
    for (final filepath in args.files) {
      final content = await File(filepath).readAsBytes();
      inputFiles.add({
        'filename': filepath.split('/').last,
        'content_base64': base64Encode(content),
      });
    }
    payload['input_files'] = inputFiles;
  }

  if (args.artifacts) {
    payload['return_artifacts'] = true;
  }
  if (args.network != null) {
    payload['network'] = args.network;
  }
  if (args.vcpu > 0) {
    payload['vcpu'] = args.vcpu;
  }

  final result = await apiRequestCurl('/execute', 'POST', jsonEncode(payload), publicKey, secretKey);

  final stdoutText = result['stdout'] as String?;
  final stderrText = result['stderr'] as String?;

  if (stdoutText != null && stdoutText.isNotEmpty) {
    stdout.write('$blue$stdoutText$reset');
  }
  if (stderrText != null && stderrText.isNotEmpty) {
    stderr.write('$red$stderrText$reset');
  }

  if (args.artifacts && result.containsKey('artifacts')) {
    final artifacts = result['artifacts'] as List;
    final outDir = args.outputDir ?? '.';
    await Directory(outDir).create(recursive: true);
    for (final artifact in artifacts) {
      final artifactMap = artifact as Map<String, dynamic>;
      final filename = artifactMap['filename'] as String? ?? 'artifact';
      final content = base64Decode(artifactMap['content_base64'] as String);
      final file = File('$outDir/$filename');
      await file.writeAsBytes(content);
      await Process.run('chmod', ['+x', file.path]);
      stderr.writeln('${green}Saved: ${file.path}$reset');
    }
  }

  final exitCode = result['exit_code'] as int? ?? 0;
  exit(exitCode);
}

Future<void> cmdSession(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  if (args.sessionList) {
    final result = await apiRequestCurl('/sessions', 'GET', null, publicKey, secretKey);
    final sessions = result['sessions'] as List? ?? [];
    if (sessions.isEmpty) {
      print('No active sessions');
    } else {
      print('${'ID'.padRight(40)} ${'Shell'.padRight(10)} ${'Status'.padRight(10)} Created');
      for (final s in sessions) {
        final session = s as Map<String, dynamic>;
        print('${(session['id'] ?? 'N/A').toString().padRight(40)} ${(session['shell'] ?? 'N/A').toString().padRight(10)} ${(session['status'] ?? 'N/A').toString().padRight(10)} ${session['created_at'] ?? 'N/A'}');
      }
    }
    return;
  }

  if (args.sessionKill != null) {
    await apiRequestCurl('/sessions/${args.sessionKill}', 'DELETE', null, publicKey, secretKey);
    print('${green}Session terminated: ${args.sessionKill}$reset');
    return;
  }

  final payload = <String, dynamic>{
    'shell': args.sessionShell ?? 'bash',
  };
  if (args.network != null) {
    payload['network'] = args.network;
  }
  if (args.vcpu > 0) {
    payload['vcpu'] = args.vcpu;
  }

  print('${yellow}Creating session...$reset');
  final result = await apiRequestCurl('/sessions', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Session created: ${result['id'] ?? 'N/A'}$reset');
  print('${yellow}(Interactive sessions require WebSocket - use un2 for full support)$reset');
}

Future<void> cmdService(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  if (args.serviceList) {
    final result = await apiRequestCurl('/services', 'GET', null, publicKey, secretKey);
    final services = result['services'] as List? ?? [];
    if (services.isEmpty) {
      print('No services');
    } else {
      print('${'ID'.padRight(20)} ${'Name'.padRight(15)} ${'Status'.padRight(10)} ${'Ports'.padRight(15)} Domains');
      for (final s in services) {
        final service = s as Map<String, dynamic>;
        final ports = (service['ports'] as List?)?.join(',') ?? '';
        final domains = (service['domains'] as List?)?.join(',') ?? '';
        print('${(service['id'] ?? 'N/A').toString().padRight(20)} ${(service['name'] ?? 'N/A').toString().padRight(15)} ${(service['status'] ?? 'N/A').toString().padRight(10)} ${ports.padRight(15)} $domains');
      }
    }
    return;
  }

  if (args.serviceInfo != null) {
    final result = await apiRequestCurl('/services/${args.serviceInfo}', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.serviceLogs != null) {
    final result = await apiRequestCurl('/services/${args.serviceLogs}/logs', 'GET', null, publicKey, secretKey);
    print(result['logs'] ?? '');
    return;
  }

  if (args.serviceTail != null) {
    final result = await apiRequestCurl('/services/${args.serviceTail}/logs?lines=9000', 'GET', null, publicKey, secretKey);
    print(result['logs'] ?? '');
    return;
  }

  if (args.serviceSleep != null) {
    await apiRequestCurl('/services/${args.serviceSleep}/sleep', 'POST', null, publicKey, secretKey);
    print('${green}Service sleeping: ${args.serviceSleep}$reset');
    return;
  }

  if (args.serviceWake != null) {
    await apiRequestCurl('/services/${args.serviceWake}/wake', 'POST', null, publicKey, secretKey);
    print('${green}Service waking: ${args.serviceWake}$reset');
    return;
  }

  if (args.serviceDestroy != null) {
    await apiRequestCurl('/services/${args.serviceDestroy}', 'DELETE', null, publicKey, secretKey);
    print('${green}Service destroyed: ${args.serviceDestroy}$reset');
    return;
  }

  if (args.serviceExecute != null) {
    final payload = <String, dynamic>{
      'command': args.serviceCommand,
    };
    final result = await apiRequestCurl('/services/${args.serviceExecute}/execute', 'POST', jsonEncode(payload), publicKey, secretKey);
    final stdoutText = result['stdout'] as String?;
    final stderrText = result['stderr'] as String?;
    if (stdoutText != null && stdoutText.isNotEmpty) {
      stdout.write('$blue$stdoutText$reset');
    }
    if (stderrText != null && stderrText.isNotEmpty) {
      stderr.write('$red$stderrText$reset');
    }
    return;
  }

  if (args.serviceDumpBootstrap != null) {
    stderr.writeln('Fetching bootstrap script from ${args.serviceDumpBootstrap}...');
    final payload = <String, dynamic>{
      'command': 'cat /tmp/bootstrap.sh',
    };
    final result = await apiRequestCurl('/services/${args.serviceDumpBootstrap}/execute', 'POST', jsonEncode(payload), publicKey, secretKey);

    final bootstrap = result['stdout'] as String?;
    if (bootstrap != null && bootstrap.isNotEmpty) {
      if (args.serviceDumpFile != null) {
        try {
          await File(args.serviceDumpFile!).writeAsString(bootstrap);
          await Process.run('chmod', ['755', args.serviceDumpFile!]);
          print('Bootstrap saved to ${args.serviceDumpFile}');
        } catch (e) {
          stderr.writeln('${red}Error: Could not write to ${args.serviceDumpFile}: $e$reset');
          exit(1);
        }
      } else {
        stdout.write(bootstrap);
      }
    } else {
      stderr.writeln('${red}Error: Failed to fetch bootstrap (service not running or no bootstrap file)$reset');
      exit(1);
    }
    return;
  }

  if (args.serviceName != null) {
    final payload = <String, dynamic>{
      'name': args.serviceName!,
    };
    if (args.servicePorts != null) {
      payload['ports'] = args.servicePorts!.split(',').map((p) => int.parse(p.trim())).toList();
    }
    if (args.serviceType != null) {
      payload['service_type'] = args.serviceType;
    }
    if (args.serviceBootstrap != null) {
      payload['bootstrap'] = args.serviceBootstrap;
    }
    if (args.network != null) {
      payload['network'] = args.network;
    }
    if (args.vcpu > 0) {
      payload['vcpu'] = args.vcpu;
    }

    final result = await apiRequestCurl('/services', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Service created: ${result['id'] ?? 'N/A'}$reset');
    print('Name: ${result['name'] ?? 'N/A'}');
    if (result.containsKey('url')) {
      print('URL: ${result['url']}');
    }
    return;
  }

  stderr.writeln('${red}Error: Specify --name to create a service, or use --list, --info, etc.$reset');
  exit(1);
}

Future<void> cmdKey(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  try {
    final result = await apiRequestCurl('/keys/validate', 'POST', null, publicKey, secretKey, baseUrl: portalBase);

    // Handle --extend flag
    if (args.keyExtend) {
      final publicKey = result['public_key'] as String?;
      if (publicKey != null) {
        final url = '$portalBase/keys/extend?pk=$publicKey';
        print('${blue}Opening browser to extend key...$reset');
        if (Platform.isMacOS) {
          await Process.run('open', [url]);
        } else if (Platform.isLinux) {
          await Process.run('xdg-open', [url]);
        } else if (Platform.isWindows) {
          await Process.run('cmd', ['/c', 'start', url]);
        } else {
          print('${yellow}Please open manually: $url$reset');
        }
        return;
      } else {
        stderr.writeln('${red}Error: Could not retrieve public key$reset');
        exit(1);
      }
    }

    // Check if key is expired
    final expired = result['expired'] as bool? ?? false;
    if (expired) {
      print('${red}Expired$reset');
      print('Public Key: ${result['public_key'] ?? 'N/A'}');
      print('Tier: ${result['tier'] ?? 'N/A'}');
      print('Expired: ${result['expires_at'] ?? 'N/A'}');
      print('${yellow}To renew: Visit $portalBase/keys/extend$reset');
      exit(1);
    }

    // Valid key
    print('${green}Valid$reset');
    print('Public Key: ${result['public_key'] ?? 'N/A'}');
    print('Tier: ${result['tier'] ?? 'N/A'}');
    print('Status: ${result['status'] ?? 'N/A'}');
    print('Expires: ${result['expires_at'] ?? 'N/A'}');
    print('Time Remaining: ${result['time_remaining'] ?? 'N/A'}');
    print('Rate Limit: ${result['rate_limit'] ?? 'N/A'}');
    print('Burst: ${result['burst'] ?? 'N/A'}');
    print('Concurrency: ${result['concurrency'] ?? 'N/A'}');
  } catch (e) {
    print('${red}Invalid$reset');
    print('Reason: $e');
    exit(1);
  }
}

Args parseArgs(List<String> argv) {
  final args = Args();
  var i = 0;
  while (i < argv.length) {
    switch (argv[i]) {
      case 'session':
        args.command = 'session';
        break;
      case 'service':
        args.command = 'service';
        break;
      case 'key':
        args.command = 'key';
        break;
      case '-k':
      case '--api-key':
        args.apiKey = argv[++i];
        break;
      case '-n':
      case '--network':
        args.network = argv[++i];
        break;
      case '-v':
      case '--vcpu':
        args.vcpu = int.parse(argv[++i]);
        break;
      case '-e':
      case '--env':
        args.env.add(argv[++i]);
        break;
      case '-f':
      case '--files':
        args.files.add(argv[++i]);
        break;
      case '-a':
      case '--artifacts':
        args.artifacts = true;
        break;
      case '-o':
      case '--output-dir':
        args.outputDir = argv[++i];
        break;
      case '-l':
      case '--list':
        if (args.command == 'session') {
          args.sessionList = true;
        } else if (args.command == 'service') {
          args.serviceList = true;
        }
        break;
      case '-s':
      case '--shell':
        args.sessionShell = argv[++i];
        break;
      case '--kill':
        args.sessionKill = argv[++i];
        break;
      case '--name':
        args.serviceName = argv[++i];
        break;
      case '--ports':
        args.servicePorts = argv[++i];
        break;
      case '--type':
        args.serviceType = argv[++i];
        break;
      case '--bootstrap':
        args.serviceBootstrap = argv[++i];
        break;
      case '--info':
        args.serviceInfo = argv[++i];
        break;
      case '--logs':
        args.serviceLogs = argv[++i];
        break;
      case '--tail':
        args.serviceTail = argv[++i];
        break;
      case '--freeze':
        args.serviceSleep = argv[++i];
        break;
      case '--unfreeze':
        args.serviceWake = argv[++i];
        break;
      case '--destroy':
        args.serviceDestroy = argv[++i];
        break;
      case '--execute':
        args.serviceExecute = argv[++i];
        break;
      case '--command':
        args.serviceCommand = argv[++i];
        break;
      case '--dump-bootstrap':
        args.serviceDumpBootstrap = argv[++i];
        break;
      case '--dump-file':
        args.serviceDumpFile = argv[++i];
        break;
      case '--extend':
        args.keyExtend = true;
        break;
      default:
        if (!argv[i].startsWith('-')) {
          args.sourceFile = argv[i];
        }
    }
    i++;
  }
  return args;
}

void printHelp() {
  print('''
Usage: dart un.dart [options] <source_file>
       dart un.dart session [options]
       dart un.dart service [options]
       dart un.dart key [options]

Execute options:
  -e KEY=VALUE      Set environment variable
  -f FILE           Add input file
  -a                Return artifacts
  -o DIR            Output directory for artifacts
  -n MODE           Network mode (zerotrust/semitrusted)
  -v N              vCPU count (1-8)
  -k KEY            API key

Session options:
  --list            List active sessions
  --shell NAME      Shell/REPL to use
  --kill ID         Terminate session

Service options:
  --list            List services
  --name NAME       Service name
  --ports PORTS     Comma-separated ports
  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)
  --bootstrap CMD   Bootstrap command
  --info ID         Get service details
  --logs ID         Get all logs
  --tail ID         Get last 9000 lines
  --freeze ID        Freeze service
  --unfreeze ID         Unfreeze service
  --destroy ID      Destroy service
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)

Key options:
  --extend          Open browser to extend key
''');
}

void main(List<String> arguments) async {
  try {
    final args = parseArgs(arguments);

    if (args.command == 'session') {
      await cmdSession(args);
    } else if (args.command == 'service') {
      await cmdService(args);
    } else if (args.command == 'key') {
      await cmdKey(args);
    } else if (args.sourceFile != null) {
      await cmdExecute(args);
    } else {
      printHelp();
      exit(1);
    }
  } catch (e) {
    stderr.writeln('${red}Error: $e$reset');
    exit(1);
  }
}
