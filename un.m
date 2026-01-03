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


#!/usr/bin/env -S clang -x objective-c -framework Foundation -o /tmp/un_objc && /tmp/un_objc

// unsandbox CLI - Objective-C implementation
// Full-featured CLI matching un.c/un.py capabilities

#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonHMAC.h>

static NSString* API_BASE = @"https://api.unsandbox.com";
static NSString* PORTAL_BASE = @"https://unsandbox.com";
static NSString* BLUE = @"\033[34m";
static NSString* RED = @"\033[31m";
static NSString* GREEN = @"\033[32m";
static NSString* YELLOW = @"\033[33m";
static NSString* RESET = @"\033[0m";

NSDictionary* getExtMap() {
    return @{
        @"py": @"python", @"js": @"javascript", @"ts": @"typescript",
        @"rb": @"ruby", @"php": @"php", @"pl": @"perl", @"lua": @"lua",
        @"sh": @"bash", @"go": @"go", @"rs": @"rust", @"c": @"c",
        @"cpp": @"cpp", @"cc": @"cpp", @"cxx": @"cpp",
        @"java": @"java", @"kt": @"kotlin", @"cs": @"csharp", @"fs": @"fsharp",
        @"hs": @"haskell", @"ml": @"ocaml", @"clj": @"clojure", @"scm": @"scheme",
        @"lisp": @"commonlisp", @"erl": @"erlang", @"ex": @"elixir", @"exs": @"elixir",
        @"jl": @"julia", @"r": @"r", @"R": @"r", @"cr": @"crystal",
        @"d": @"d", @"nim": @"nim", @"zig": @"zig", @"v": @"vlang",
        @"dart": @"dart", @"groovy": @"groovy", @"scala": @"scala",
        @"f90": @"fortran", @"f95": @"fortran", @"cob": @"cobol",
        @"pro": @"prolog", @"forth": @"forth", @"4th": @"forth",
        @"tcl": @"tcl", @"raku": @"raku", @"pl6": @"raku", @"p6": @"raku",
        @"m": @"objc"
    };
}

void getApiKeys(NSString** publicKey, NSString** secretKey) {
    // Try new-style keys first
    *publicKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_PUBLIC_KEY"];
    *secretKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_SECRET_KEY"];

    // Fall back to old-style single key
    if (!*publicKey || [*publicKey length] == 0) {
        NSString* oldKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_API_KEY"];
        if (!oldKey) {
            fprintf(stderr, "%sError: UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set%s\n",
                    [RED UTF8String], [RESET UTF8String]);
            exit(1);
        }
        *publicKey = oldKey;
        *secretKey = oldKey;
        return;
    }

    if (!*secretKey || [*secretKey length] == 0) {
        fprintf(stderr, "%sError: UNSANDBOX_SECRET_KEY not set%s\n", [RED UTF8String], [RESET UTF8String]);
        exit(1);
    }
}

void checkClockDrift(NSString* response) {
    NSString* responseLower = [response lowercaseString];
    if ([responseLower rangeOfString:@"timestamp"].location != NSNotFound &&
        ([responseLower rangeOfString:@"401"].location != NSNotFound ||
         [responseLower rangeOfString:@"expired"].location != NSNotFound ||
         [responseLower rangeOfString:@"invalid"].location != NSNotFound)) {
        fprintf(stderr, "%sError: Request timestamp expired (must be within 5 minutes of server time)%s\n",
                [RED UTF8String], [RESET UTF8String]);
        fprintf(stderr, "%sYour computer's clock may have drifted.%s\n",
                [YELLOW UTF8String], [RESET UTF8String]);
        fprintf(stderr, "Check your system time and sync with NTP if needed:\n");
        fprintf(stderr, "  Linux:   sudo ntpdate -s time.nist.gov\n");
        fprintf(stderr, "  macOS:   sudo sntp -sS time.apple.com\n");
        fprintf(stderr, "  Windows: w32tm /resync%s\n", [RESET UTF8String]);
        exit(1);
    }
}

NSString* hmacSha256Hex(NSString* key, NSString* message) {
    const char* cKey = [key UTF8String];
    const char* cMessage = [message UTF8String];
    unsigned char digest[CC_SHA256_DIGEST_LENGTH];

    CCHmac(kCCHmacAlgSHA256, cKey, strlen(cKey), cMessage, strlen(cMessage), digest);

    NSMutableString* hex = [NSMutableString stringWithCapacity:CC_SHA256_DIGEST_LENGTH * 2];
    for (int i = 0; i < CC_SHA256_DIGEST_LENGTH; i++) {
        [hex appendFormat:@"%02x", digest[i]];
    }
    return hex;
}

NSString* computeSignature(NSString* secretKey, long timestamp, NSString* method, NSString* path, NSString* body) {
    NSString* message = [NSString stringWithFormat:@"%ld:%@:%@:%@", timestamp, method, path, body];
    return hmacSha256Hex(secretKey, message);
}

NSString* detectLanguage(NSString* filename) {
    NSString* ext = [filename pathExtension];
    NSDictionary* langMap = getExtMap();

    NSString* language = langMap[ext];
    if (!language) {
        fprintf(stderr, "%sError: Cannot detect language for %s%s\n",
                [RED UTF8String], [filename UTF8String], [RESET UTF8String]);
        exit(1);
    }

    return language;
}

NSDictionary* apiRequest(NSString* endpoint, NSString* method, NSDictionary* data, NSString* publicKey, NSString* secretKey) {
    NSString* urlString = [API_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:method];
    [request setTimeoutInterval:300];

    // Prepare body
    NSString* bodyString = @"";
    if (data) {
        NSError* error = nil;
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:data options:0 error:&error];
        if (error) {
            fprintf(stderr, "%sError creating JSON: %s%s\n",
                    [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
            exit(1);
        }
        bodyString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [request setHTTPBody:jsonData];
    }

    // Generate timestamp and signature
    long timestamp = (long)[[NSDate date] timeIntervalSince1970];
    NSString* signature = computeSignature(secretKey, timestamp, method, endpoint, bodyString);

    // Set headers
    [request setValue:[@"Bearer " stringByAppendingString:publicKey] forHTTPHeaderField:@"Authorization"];
    [request setValue:[NSString stringWithFormat:@"%ld", timestamp] forHTTPHeaderField:@"X-Timestamp"];
    [request setValue:signature forHTTPHeaderField:@"X-Signature"];
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];

    NSHTTPURLResponse* response = nil;
    NSError* error = nil;
    NSData* responseData = [NSURLConnection sendSynchronousRequest:request
                                                 returningResponse:&response
                                                             error:&error];

    if (error || ([response statusCode] != 200 && [response statusCode] != 201)) {
        fprintf(stderr, "%sError: HTTP %ld%s\n",
                [RED UTF8String], (long)[response statusCode], [RESET UTF8String]);
        if (responseData) {
            NSString* errMsg = [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding];
            fprintf(stderr, "%s\n", [errMsg UTF8String]);
            checkClockDrift(errMsg);
        }
        exit(1);
    }

    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
    if (error) {
        fprintf(stderr, "%sError parsing JSON: %s%s\n",
                [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
        exit(1);
    }

    return result;
}

void cmdExecute(NSArray* args) {
    NSString* publicKey, *secretKey;
    getApiKeys(&publicKey, &secretKey);
    NSString* sourceFile = nil;
    NSMutableDictionary* envVars = [NSMutableDictionary dictionary];
    NSMutableArray* inputFiles = [NSMutableArray array];
    BOOL artifacts = NO;
    NSString* outputDir = @".";
    NSString* network = nil;
    int vcpu = 0;

    // Parse arguments
    for (NSUInteger i = 0; i < [args count]; i++) {
        NSString* arg = args[i];
        if ([arg isEqualToString:@"-e"] && i + 1 < [args count]) {
            NSArray* parts = [args[++i] componentsSeparatedByString:@"="];
            if ([parts count] >= 2) {
                envVars[parts[0]] = [parts subarrayWithRange:NSMakeRange(1, [parts count] - 1)].componentsJoinedByString:@"=";
            }
        } else if ([arg isEqualToString:@"-f"] && i + 1 < [args count]) {
            [inputFiles addObject:args[++i]];
        } else if ([arg isEqualToString:@"-a"]) {
            artifacts = YES;
        } else if ([arg isEqualToString:@"-o"] && i + 1 < [args count]) {
            outputDir = args[++i];
        } else if ([arg isEqualToString:@"-n"] && i + 1 < [args count]) {
            network = args[++i];
        } else if ([arg isEqualToString:@"-v"] && i + 1 < [args count]) {
            vcpu = [args[++i] intValue];
        } else if ([arg hasPrefix:@"-"]) {
            fprintf(stderr, "%sUnknown option: %s%s\n", RED, [arg UTF8String], RESET);
            exit(1);
        } else {
            sourceFile = arg;
        }
    }

    if (!sourceFile) {
        fprintf(stderr, "Usage: un.m [options] <source_file>\n");
        exit(1);
    }

    NSFileManager* fm = [NSFileManager defaultManager];
    if (![fm fileExistsAtPath:sourceFile]) {
        fprintf(stderr, "%sError: File not found: %s%s\n",
                [RED UTF8String], [sourceFile UTF8String], [RESET UTF8String]);
        exit(1);
    }

    // Read source file
    NSError* error = nil;
    NSString* code = [NSString stringWithContentsOfFile:sourceFile encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        fprintf(stderr, "%sError reading file: %s%s\n",
                [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
        exit(1);
    }

    NSString* language = detectLanguage(sourceFile);

    // Build request payload
    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"language": language,
        @"code": code
    }];

    if ([envVars count] > 0) {
        payload[@"env"] = envVars;
    }

    if ([inputFiles count] > 0) {
        NSMutableArray* files = [NSMutableArray array];
        for (NSString* filepath in inputFiles) {
            if (![fm fileExistsAtPath:filepath]) {
                fprintf(stderr, "%sError: Input file not found: %s%s\n",
                        [RED UTF8String], [filepath UTF8String], [RESET UTF8String]);
                exit(1);
            }
            NSData* content = [NSData dataWithContentsOfFile:filepath];
            NSString* b64Content = [content base64EncodedStringWithOptions:0];
            [files addObject:@{
                @"filename": [filepath lastPathComponent],
                @"content_base64": b64Content
            }];
        }
        payload[@"input_files"] = files;
    }

    if (artifacts) {
        payload[@"return_artifacts"] = @YES;
    }
    if (network) {
        payload[@"network"] = network;
    }
    if (vcpu > 0) {
        payload[@"vcpu"] = @(vcpu);
    }

    // Execute
    NSDictionary* result = apiRequest(@"/execute", @"POST", payload, publicKey, secretKey);

    // Print output
    NSString* stdoutText = result[@"stdout"] ?: @"";
    NSString* stderrText = result[@"stderr"] ?: @"";

    if ([stdoutText length] > 0) {
        printf("%s%s%s", [BLUE UTF8String], [stdoutText UTF8String], [RESET UTF8String]);
    }
    if ([stderrText length] > 0) {
        fprintf(stderr, "%s%s%s", [RED UTF8String], [stderrText UTF8String], [RESET UTF8String]);
    }

    // Save artifacts
    if (artifacts && result[@"artifacts"]) {
        [fm createDirectoryAtPath:outputDir withIntermediateDirectories:YES attributes:nil error:nil];
        for (NSDictionary* artifact in result[@"artifacts"]) {
            NSString* filename = artifact[@"filename"];
            NSString* b64Content = artifact[@"content_base64"];
            NSData* content = [[NSData alloc] initWithBase64EncodedString:b64Content options:0];
            NSString* path = [outputDir stringByAppendingPathComponent:filename];
            [content writeToFile:path atomically:YES];
            [fm setAttributes:@{NSFilePosixPermissions: @0755} ofItemAtPath:path error:nil];
            fprintf(stderr, "%sSaved: %s%s\n", [GREEN UTF8String], [path UTF8String], [RESET UTF8String]);
        }
    }

    int exitCode = [result[@"exit_code"] intValue];
    exit(exitCode);
}

NSDictionary* portalRequest(NSString* endpoint, NSString* method, NSDictionary* data, NSString* apiKey) {
    NSString* urlString = [PORTAL_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:method];
    [request setValue:[@"Bearer " stringByAppendingString:apiKey] forHTTPHeaderField:@"Authorization"];
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    [request setTimeoutInterval:30];

    if (data) {
        NSError* error = nil;
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:data options:0 error:&error];
        if (error) {
            fprintf(stderr, "%sError creating JSON: %s%s\n",
                    [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
            exit(1);
        }
        [request setHTTPBody:jsonData];
    }

    NSHTTPURLResponse* response = nil;
    NSError* error = nil;
    NSData* responseData = [NSURLConnection sendSynchronousRequest:request
                                                 returningResponse:&response
                                                             error:&error];

    if (error || [response statusCode] >= 400) {
        // For key validation, return the parsed JSON even on error
        if (responseData) {
            NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
            if (result) {
                return result;
            }
        }
        fprintf(stderr, "%sError: HTTP %ld%s\n",
                [RED UTF8String], (long)[response statusCode], [RESET UTF8String]);
        if (responseData) {
            NSString* errMsg = [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding];
            fprintf(stderr, "%s\n", [errMsg UTF8String]);
            checkClockDrift(errMsg);
        }
        exit(1);
    }

    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
    if (error) {
        fprintf(stderr, "%sError parsing JSON: %s%s\n",
                [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
        exit(1);
    }

    return result;
}

void openBrowser(NSString* url) {
    NSString* command = [NSString stringWithFormat:@"open \"%@\"", url];
    system([command UTF8String]);
}

void validateKey(NSString* apiKey, BOOL shouldExtend) {
    NSDictionary* result = portalRequest(@"/keys/validate", @"POST", @{}, apiKey);

    if ([result[@"expired"] boolValue]) {
        printf("%sExpired%s\n", [RED UTF8String], [RESET UTF8String]);
        printf("Public Key: %s\n", [result[@"public_key"] UTF8String] ?: "N/A");
        printf("Tier: %s\n", [result[@"tier"] UTF8String] ?: "N/A");
        printf("Expired: %s\n", [result[@"expires_at"] UTF8String] ?: "N/A");
        printf("%sTo renew: Visit https://unsandbox.com/keys/extend%s\n",
               [YELLOW UTF8String], [RESET UTF8String]);

        if (shouldExtend && result[@"public_key"]) {
            NSString* extendUrl = [NSString stringWithFormat:@"%@/keys/extend?pk=%@",
                                   PORTAL_BASE, result[@"public_key"]];
            printf("\n%sOpening browser to extend key...%s\n", [BLUE UTF8String], [RESET UTF8String]);
            openBrowser(extendUrl);
        }
        exit(1);
    }

    printf("%sValid%s\n", [GREEN UTF8String], [RESET UTF8String]);
    printf("Public Key: %s\n", [result[@"public_key"] UTF8String] ?: "N/A");
    printf("Tier: %s\n", [result[@"tier"] UTF8String] ?: "N/A");
    printf("Status: %s\n", [result[@"status"] UTF8String] ?: "N/A");
    printf("Expires: %s\n", [result[@"expires_at"] UTF8String] ?: "N/A");
    printf("Time Remaining: %s\n", [result[@"time_remaining"] UTF8String] ?: "N/A");
    printf("Rate Limit: %s\n", [result[@"rate_limit"] UTF8String] ?: "N/A");
    printf("Burst: %s\n", [result[@"burst"] UTF8String] ?: "N/A");
    printf("Concurrency: %s\n", [result[@"concurrency"] UTF8String] ?: "N/A");

    if (shouldExtend && result[@"public_key"]) {
        NSString* extendUrl = [NSString stringWithFormat:@"%@/keys/extend?pk=%@",
                               PORTAL_BASE, result[@"public_key"]];
        printf("\n%sOpening browser to extend key...%s\n", [BLUE UTF8String], [RESET UTF8String]);
        openBrowser(extendUrl);
    }
}

void cmdKey(NSArray* args) {
    NSString* publicKey, *secretKey;
    getApiKeys(&publicKey, &secretKey);
    BOOL shouldExtend = NO;

    for (NSString* arg in args) {
        if ([arg isEqualToString:@"--extend"]) {
            shouldExtend = YES;
        }
    }

    // For portal validation, we use public key as bearer token
    validateKey(publicKey, shouldExtend);
}

void cmdSession(NSArray* args) {
    NSString* publicKey, *secretKey;
    getApiKeys(&publicKey, &secretKey);
    BOOL listMode = NO;
    NSString* killId = nil;
    NSString* shell = nil;
    NSString* network = nil;
    int vcpu = 0;
    NSMutableArray* inputFiles = [NSMutableArray array];

    // Parse arguments
    for (NSUInteger i = 0; i < [args count]; i++) {
        NSString* arg = args[i];
        if ([arg isEqualToString:@"--list"]) {
            listMode = YES;
        } else if ([arg isEqualToString:@"--kill"] && i + 1 < [args count]) {
            killId = args[++i];
        } else if ([arg isEqualToString:@"--shell"] && i + 1 < [args count]) {
            shell = args[++i];
        } else if ([arg isEqualToString:@"-f"] && i + 1 < [args count]) {
            [inputFiles addObject:args[++i]];
        } else if ([arg isEqualToString:@"-n"] && i + 1 < [args count]) {
            network = args[++i];
        } else if ([arg isEqualToString:@"-v"] && i + 1 < [args count]) {
            vcpu = [args[++i] intValue];
        }
    }

    if (listMode) {
        NSDictionary* result = apiRequest(@"/sessions", @"GET", nil, publicKey, secretKey);
        NSArray* sessions = result[@"sessions"];
        if ([sessions count] == 0) {
            printf("No active sessions\n");
        } else {
            printf("%-40s %-10s %-10s %s\n", "ID", "Shell", "Status", "Created");
            for (NSDictionary* s in sessions) {
                printf("%-40s %-10s %-10s %s\n",
                       [s[@"id"] UTF8String],
                       [s[@"shell"] UTF8String],
                       [s[@"status"] UTF8String],
                       [s[@"created_at"] UTF8String]);
            }
        }
        return;
    }

    if (killId) {
        NSString* endpoint = [NSString stringWithFormat:@"/sessions/%@", killId];
        apiRequest(endpoint, @"DELETE", nil, publicKey, secretKey);
        printf("%sSession terminated: %s%s\n", [GREEN UTF8String], [killId UTF8String], [RESET UTF8String]);
        return;
    }

    // Create new session
    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"shell": shell ?: @"bash"
    }];
    if (network) payload[@"network"] = network;
    if (vcpu > 0) payload[@"vcpu"] = @(vcpu);

    // Add input files
    if ([inputFiles count] > 0) {
        NSFileManager* fm = [NSFileManager defaultManager];
        NSMutableArray* files = [NSMutableArray array];
        for (NSString* filepath in inputFiles) {
            if (![fm fileExistsAtPath:filepath]) {
                fprintf(stderr, "%sError: Input file not found: %s%s\n",
                        [RED UTF8String], [filepath UTF8String], [RESET UTF8String]);
                exit(1);
            }
            NSData* content = [NSData dataWithContentsOfFile:filepath];
            NSString* b64Content = [content base64EncodedStringWithOptions:0];
            [files addObject:@{
                @"filename": [filepath lastPathComponent],
                @"content_base64": b64Content
            }];
        }
        payload[@"input_files"] = files;
    }

    printf("%sCreating session...%s\n", [YELLOW UTF8String], [RESET UTF8String]);
    NSDictionary* result = apiRequest(@"/sessions", @"POST", payload, publicKey, secretKey);
    printf("%sSession created: %s%s\n", [GREEN UTF8String], [result[@"id"] UTF8String], [RESET UTF8String]);
    printf("%s(Interactive sessions require WebSocket - use un2 for full support)%s\n",
           [YELLOW UTF8String], [RESET UTF8String]);
}

void cmdService(NSArray* args) {
    NSString* publicKey, *secretKey;
    getApiKeys(&publicKey, &secretKey);
    BOOL listMode = NO;
    NSString* infoId = nil;
    NSString* logsId = nil;
    NSString* sleepId = nil;
    NSString* wakeId = nil;
    NSString* destroyId = nil;
    NSString* dumpBootstrapId = nil;
    NSString* dumpFile = nil;
    NSString* name = nil;
    NSString* ports = nil;
    NSString* type = nil;
    NSString* bootstrap = nil;
    NSString* bootstrapFile = nil;
    NSString* network = nil;
    int vcpu = 0;
    NSMutableArray* inputFiles = [NSMutableArray array];

    // Parse arguments
    for (NSUInteger i = 0; i < [args count]; i++) {
        NSString* arg = args[i];
        if ([arg isEqualToString:@"--list"]) {
            listMode = YES;
        } else if ([arg isEqualToString:@"--info"] && i + 1 < [args count]) {
            infoId = args[++i];
        } else if ([arg isEqualToString:@"--logs"] && i + 1 < [args count]) {
            logsId = args[++i];
        } else if ([arg isEqualToString:@"--freeze"] && i + 1 < [args count]) {
            sleepId = args[++i];
        } else if ([arg isEqualToString:@"--unfreeze"] && i + 1 < [args count]) {
            wakeId = args[++i];
        } else if ([arg isEqualToString:@"--destroy"] && i + 1 < [args count]) {
            destroyId = args[++i];
        } else if ([arg isEqualToString:@"--dump-bootstrap"] && i + 1 < [args count]) {
            dumpBootstrapId = args[++i];
        } else if ([arg isEqualToString:@"--dump-file"] && i + 1 < [args count]) {
            dumpFile = args[++i];
        } else if ([arg isEqualToString:@"--name"] && i + 1 < [args count]) {
            name = args[++i];
        } else if ([arg isEqualToString:@"--ports"] && i + 1 < [args count]) {
            ports = args[++i];
        } else if ([arg isEqualToString:@"--type"] && i + 1 < [args count]) {
            type = args[++i];
        } else if ([arg isEqualToString:@"--bootstrap"] && i + 1 < [args count]) {
            bootstrap = args[++i];
        } else if ([arg isEqualToString:@"--bootstrap-file"] && i + 1 < [args count]) {
            bootstrapFile = args[++i];
        } else if ([arg isEqualToString:@"-f"] && i + 1 < [args count]) {
            [inputFiles addObject:args[++i]];
        } else if ([arg isEqualToString:@"-n"] && i + 1 < [args count]) {
            network = args[++i];
        } else if ([arg isEqualToString:@"-v"] && i + 1 < [args count]) {
            vcpu = [args[++i] intValue];
        }
    }

    if (listMode) {
        NSDictionary* result = apiRequest(@"/services", @"GET", nil, publicKey, secretKey);
        NSArray* services = result[@"services"];
        if ([services count] == 0) {
            printf("No services\n");
        } else {
            printf("%-20s %-15s %-10s %-15s %s\n", "ID", "Name", "Status", "Ports", "Domains");
            for (NSDictionary* s in services) {
                NSArray* portArray = s[@"ports"];
                NSArray* domainArray = s[@"domains"];
                NSString* portStr = [portArray componentsJoinedByString:@","];
                NSString* domainStr = [domainArray componentsJoinedByString:@","];
                printf("%-20s %-15s %-10s %-15s %s\n",
                       [s[@"id"] UTF8String],
                       [s[@"name"] UTF8String],
                       [s[@"status"] UTF8String],
                       [portStr UTF8String],
                       [domainStr UTF8String]);
            }
        }
        return;
    }

    if (infoId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@", infoId];
        NSDictionary* result = apiRequest(endpoint, @"GET", nil, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (logsId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/logs", logsId];
        NSDictionary* result = apiRequest(endpoint, @"GET", nil, publicKey, secretKey);
        printf("%s", [result[@"logs"] UTF8String]);
        return;
    }

    if (sleepId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/sleep", sleepId];
        apiRequest(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sService sleeping: %s%s\n", [GREEN UTF8String], [sleepId UTF8String], [RESET UTF8String]);
        return;
    }

    if (wakeId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/wake", wakeId];
        apiRequest(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sService waking: %s%s\n", [GREEN UTF8String], [wakeId UTF8String], [RESET UTF8String]);
        return;
    }

    if (destroyId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@", destroyId];
        apiRequest(endpoint, @"DELETE", nil, publicKey, secretKey);
        printf("%sService destroyed: %s%s\n", [GREEN UTF8String], [destroyId UTF8String], [RESET UTF8String]);
        return;
    }

    if (dumpBootstrapId) {
        fprintf(stderr, "Fetching bootstrap script from %s...\n", [dumpBootstrapId UTF8String]);
        NSDictionary* payload = @{@"command": @"cat /tmp/bootstrap.sh"};
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/execute", dumpBootstrapId];
        NSDictionary* result = apiRequest(endpoint, @"POST", payload, publicKey, secretKey);

        if (result[@"stdout"] && [result[@"stdout"] length] > 0) {
            NSString* bootstrap = result[@"stdout"];
            if (dumpFile) {
                // Write to file
                NSError* error = nil;
                [bootstrap writeToFile:dumpFile atomically:YES encoding:NSUTF8StringEncoding error:&error];
                if (error) {
                    fprintf(stderr, "%sError: Could not write to %s: %s%s\n",
                            [RED UTF8String], [dumpFile UTF8String],
                            [[error localizedDescription] UTF8String], [RESET UTF8String]);
                    exit(1);
                }
                NSFileManager* fm = [NSFileManager defaultManager];
                [fm setAttributes:@{NSFilePosixPermissions: @0755} ofItemAtPath:dumpFile error:nil];
                printf("Bootstrap saved to %s\n", [dumpFile UTF8String]);
            } else {
                // Print to stdout
                printf("%s", [bootstrap UTF8String]);
            }
        } else {
            fprintf(stderr, "%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s\n",
                    [RED UTF8String], [RESET UTF8String]);
            exit(1);
        }
        return;
    }

    // Create new service
    if (name) {
        NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{@"name": name}];

        if (ports) {
            NSArray* portStrings = [ports componentsSeparatedByString:@","];
            NSMutableArray* portNumbers = [NSMutableArray array];
            for (NSString* p in portStrings) {
                [portNumbers addObject:@([p intValue])];
            }
            payload[@"ports"] = portNumbers;
        }

        if (type) {
            payload[@"service_type"] = type;
        }

        if (bootstrap) {
            payload[@"bootstrap"] = bootstrap;
        }

        if (bootstrapFile) {
            NSFileManager* fm = [NSFileManager defaultManager];
            if ([fm fileExistsAtPath:bootstrapFile]) {
                NSString* content = [NSString stringWithContentsOfFile:bootstrapFile encoding:NSUTF8StringEncoding error:nil];
                payload[@"bootstrap_content"] = content;
            } else {
                fprintf(stderr, "%sError: Bootstrap file not found: %s%s\n",
                        [RED UTF8String], [bootstrapFile UTF8String], [RESET UTF8String]);
                exit(1);
            }
        }

        // Add input files
        if ([inputFiles count] > 0) {
            NSFileManager* fm = [NSFileManager defaultManager];
            NSMutableArray* files = [NSMutableArray array];
            for (NSString* filepath in inputFiles) {
                if (![fm fileExistsAtPath:filepath]) {
                    fprintf(stderr, "%sError: Input file not found: %s%s\n",
                            [RED UTF8String], [filepath UTF8String], [RESET UTF8String]);
                    exit(1);
                }
                NSData* content = [NSData dataWithContentsOfFile:filepath];
                NSString* b64Content = [content base64EncodedStringWithOptions:0];
                [files addObject:@{
                    @"filename": [filepath lastPathComponent],
                    @"content_base64": b64Content
                }];
            }
            payload[@"input_files"] = files;
        }

        if (network) payload[@"network"] = network;
        if (vcpu > 0) payload[@"vcpu"] = @(vcpu);

        NSDictionary* result = apiRequest(@"/services", @"POST", payload, publicKey, secretKey);
        printf("%sService created: %s%s\n", [GREEN UTF8String], [result[@"id"] UTF8String], [RESET UTF8String]);
        printf("Name: %s\n", [result[@"name"] UTF8String]);
        if (result[@"url"]) {
            printf("URL: %s\n", [result[@"url"] UTF8String]);
        }
        return;
    }

    fprintf(stderr, "%sError: Specify --name to create a service, or use --list, --info, etc.%s\n",
            [RED UTF8String], [RESET UTF8String]);
    exit(1);
}

int main(int argc, const char* argv[]) {
    @autoreleasepool {
        if (argc < 2) {
            fprintf(stderr, "Usage: un.m [options] <source_file>\n");
            fprintf(stderr, "       un.m session [options]\n");
            fprintf(stderr, "       un.m service [options]\n");
            fprintf(stderr, "       un.m key [options]\n");
            return 1;
        }

        NSMutableArray* args = [NSMutableArray array];
        for (int i = 1; i < argc; i++) {
            [args addObject:[NSString stringWithUTF8String:argv[i]]];
        }

        NSString* firstArg = args[0];

        if ([firstArg isEqualToString:@"session"]) {
            cmdSession([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else if ([firstArg isEqualToString:@"service"]) {
            cmdService([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else if ([firstArg isEqualToString:@"key"]) {
            cmdKey([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else {
            cmdExecute(args);
        }
    }

    return 0;
}
