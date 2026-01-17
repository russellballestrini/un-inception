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
//
// unsandbox SDK for Objective-C - Execute code in secure sandboxes
// https://unsandbox.com | https://api.unsandbox.com/openapi
//
// Library Usage:
//   #import "un.m"  // or as header
//   UNClient *client = [[UNClient alloc] init];
//   NSDictionary *result = [client execute:@"python" code:@"print('Hello')"];
//   NSLog(@"%@", result[@"stdout"]);
//
// CLI Usage:
//   ./un.m script.py
//   ./un.m -s python 'print("Hello")'
//   ./un.m session --shell python3
//
// Authentication (in priority order):
//   1. UNClient initWithPublicKey:secretKey: constructor arguments
//   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
//   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

#!/usr/bin/env -S clang -x objective-c -framework Foundation -o /tmp/un_objc && /tmp/un_objc

#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonHMAC.h>

// ============================================================================
// Configuration
// ============================================================================

static NSString* const UN_API_BASE = @"https://api.unsandbox.com";
static NSString* const UN_PORTAL_BASE = @"https://unsandbox.com";
static const NSInteger UN_DEFAULT_TIMEOUT = 300;
static const NSInteger UN_DEFAULT_TTL = 60;
static const NSInteger UN_LANGUAGES_CACHE_TTL = 3600;  // 1 hour in seconds

// Polling delays (ms) - exponential backoff
static const int UN_POLL_DELAYS[] = {300, 450, 700, 900, 650, 1600, 2000};
static const int UN_POLL_DELAYS_COUNT = 7;

// ANSI colors
static NSString* const BLUE = @"\033[34m";
static NSString* const RED = @"\033[31m";
static NSString* const GREEN = @"\033[32m";
static NSString* const YELLOW = @"\033[33m";
static NSString* const RESET = @"\033[0m";

// ============================================================================
// Extension to Language Mapping
// ============================================================================

/**
 * Returns mapping from file extensions to language identifiers.
 */
NSDictionary* UNGetExtMap(void) {
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
        @"m": @"objc", @"awk": @"awk"
    };
}

// ============================================================================
// Error Classes
// ============================================================================

/**
 * UNError - Base error class for unsandbox SDK errors.
 */
@interface UNError : NSError
+ (instancetype)errorWithMessage:(NSString*)message;
@end

@implementation UNError
+ (instancetype)errorWithMessage:(NSString*)message {
    return [self errorWithDomain:@"com.unsandbox" code:1 userInfo:@{NSLocalizedDescriptionKey: message}];
}
@end

/**
 * UNAuthenticationError - Invalid or missing credentials.
 */
@interface UNAuthenticationError : UNError
@end

@implementation UNAuthenticationError
@end

/**
 * UNExecutionError - Code execution failed.
 */
@interface UNExecutionError : UNError
@property (nonatomic) int exitCode;
@property (nonatomic, strong) NSString* stderr;
@end

@implementation UNExecutionError
@end

/**
 * UNAPIError - API request failed.
 */
@interface UNAPIError : UNError
@property (nonatomic) NSInteger statusCode;
@property (nonatomic, strong) NSString* response;
@end

@implementation UNAPIError
@end

/**
 * UNTimeoutError - Execution or polling timed out.
 */
@interface UNTimeoutError : UNError
@end

@implementation UNTimeoutError
@end

// ============================================================================
// HMAC Authentication
// ============================================================================

/**
 * Generate HMAC-SHA256 signature in hex format.
 *
 * @param key The secret key for HMAC
 * @param message The message to sign
 * @return Hex-encoded signature string
 */
NSString* UNHmacSha256Hex(NSString* key, NSString* message) {
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

/**
 * Compute API request signature.
 * Signature = HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
 *
 * @param secretKey API secret key
 * @param timestamp Unix timestamp
 * @param method HTTP method (GET, POST, etc.)
 * @param path API endpoint path
 * @param body Request body (empty string if none)
 * @return Hex-encoded signature
 */
NSString* UNComputeSignature(NSString* secretKey, long timestamp, NSString* method, NSString* path, NSString* body) {
    NSString* message = [NSString stringWithFormat:@"%ld:%@:%@:%@", timestamp, method, path, body ?: @""];
    return UNHmacSha256Hex(secretKey, message);
}

// ============================================================================
// Credentials Loading
// ============================================================================

/**
 * Get API credentials from environment or config file.
 * Priority: 1. Arguments, 2. Environment vars, 3. ~/.unsandbox/accounts.csv
 *
 * @param publicKey Output public key
 * @param secretKey Output secret key
 * @param argPublicKey Optional public key from arguments
 * @param argSecretKey Optional secret key from arguments
 * @param error Error output
 * @return YES if credentials found, NO otherwise
 */
BOOL UNGetCredentials(NSString** publicKey, NSString** secretKey, NSString* argPublicKey, NSString* argSecretKey, NSError** error) {
    // Priority 1: Function arguments
    if (argPublicKey && argSecretKey && [argPublicKey length] > 0 && [argSecretKey length] > 0) {
        *publicKey = argPublicKey;
        *secretKey = argSecretKey;
        return YES;
    }

    // Priority 2: Environment variables
    *publicKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_PUBLIC_KEY"];
    *secretKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_SECRET_KEY"];

    if (*publicKey && *secretKey && [*publicKey length] > 0 && [*secretKey length] > 0) {
        return YES;
    }

    // Fall back to legacy UNSANDBOX_API_KEY
    NSString* oldKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"UNSANDBOX_API_KEY"];
    if (oldKey && [oldKey length] > 0) {
        *publicKey = oldKey;
        *secretKey = oldKey;
        return YES;
    }

    // Priority 3: Config file ~/.unsandbox/accounts.csv
    NSString* home = NSHomeDirectory();
    NSString* accountsPath = [home stringByAppendingPathComponent:@".unsandbox/accounts.csv"];
    NSFileManager* fm = [NSFileManager defaultManager];

    if ([fm fileExistsAtPath:accountsPath]) {
        NSString* content = [NSString stringWithContentsOfFile:accountsPath encoding:NSUTF8StringEncoding error:nil];
        if (content) {
            NSArray* lines = [content componentsSeparatedByString:@"\n"];
            for (NSString* line in lines) {
                NSString* trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
                if ([trimmed length] == 0 || [trimmed hasPrefix:@"#"]) continue;

                NSArray* parts = [trimmed componentsSeparatedByString:@","];
                if ([parts count] >= 2) {
                    NSString* pk = [parts[0] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                    NSString* sk = [parts[1] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                    if ([pk hasPrefix:@"unsb-pk-"] && [sk hasPrefix:@"unsb-sk-"]) {
                        *publicKey = pk;
                        *secretKey = sk;
                        return YES;
                    }
                }
            }
        }
    }

    if (error) {
        *error = [UNAuthenticationError errorWithMessage:
            @"No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, "
            "or create ~/.unsandbox/accounts.csv, or pass credentials to initializer."];
    }
    return NO;
}

/**
 * Get API keys for CLI commands (exits on failure).
 */
void UNGetApiKeysCLI(NSString** publicKey, NSString** secretKey) {
    NSError* error = nil;
    if (!UNGetCredentials(publicKey, secretKey, nil, nil, &error)) {
        fprintf(stderr, "%s%s%s\n", [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
        exit(1);
    }
}

// ============================================================================
// Clock Drift Detection
// ============================================================================

/**
 * Check response for timestamp/clock drift errors.
 */
void UNCheckClockDrift(NSString* response) {
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
        fprintf(stderr, "  Windows: w32tm /resync\n");
        exit(1);
    }
}

// ============================================================================
// Languages Cache
// ============================================================================

/**
 * Get path to languages cache file.
 */
NSString* UNLanguagesCachePath(void) {
    NSString* home = NSHomeDirectory();
    return [home stringByAppendingPathComponent:@".unsandbox/languages.json"];
}

/**
 * Check if languages cache is valid (less than 1 hour old).
 */
BOOL UNIsCacheValid(void) {
    NSFileManager* fm = [NSFileManager defaultManager];
    NSString* cachePath = UNLanguagesCachePath();

    if (![fm fileExistsAtPath:cachePath]) {
        return NO;
    }

    NSError* error = nil;
    NSDictionary* attrs = [fm attributesOfItemAtPath:cachePath error:&error];
    if (error) {
        return NO;
    }

    NSDate* modDate = attrs[NSFileModificationDate];
    NSTimeInterval age = -[modDate timeIntervalSinceNow];
    return age < UN_LANGUAGES_CACHE_TTL;
}

/**
 * Read languages from cache file.
 */
NSDictionary* UNReadLanguagesCache(void) {
    NSString* cachePath = UNLanguagesCachePath();
    NSFileManager* fm = [NSFileManager defaultManager];

    if (![fm fileExistsAtPath:cachePath]) {
        return nil;
    }

    NSData* data = [NSData dataWithContentsOfFile:cachePath];
    if (!data) {
        return nil;
    }

    NSError* error = nil;
    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:data options:0 error:&error];
    return error ? nil : result;
}

/**
 * Write languages to cache file.
 */
void UNWriteLanguagesCache(NSDictionary* data) {
    NSString* cachePath = UNLanguagesCachePath();
    NSString* cacheDir = [cachePath stringByDeletingLastPathComponent];
    NSFileManager* fm = [NSFileManager defaultManager];

    // Create directory if needed
    if (![fm fileExistsAtPath:cacheDir]) {
        [fm createDirectoryAtPath:cacheDir withIntermediateDirectories:YES attributes:nil error:nil];
    }

    NSError* error = nil;
    NSData* jsonData = [NSJSONSerialization dataWithJSONObject:data options:0 error:&error];
    if (!error && jsonData) {
        [jsonData writeToFile:cachePath atomically:YES];
    }
}

// ============================================================================
// Language Detection
// ============================================================================

/**
 * Detect programming language from file extension or shebang.
 *
 * @param filename Path to source file
 * @return Language identifier or nil if undetected
 */
NSString* UNDetectLanguage(NSString* filename) {
    NSString* ext = [filename pathExtension];
    NSDictionary* langMap = UNGetExtMap();

    NSString* language = langMap[ext];
    if (language) {
        return language;
    }

    // Try reading shebang
    NSFileManager* fm = [NSFileManager defaultManager];
    if ([fm fileExistsAtPath:filename]) {
        NSString* content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
        if (content) {
            NSString* firstLine = [[content componentsSeparatedByString:@"\n"] firstObject];
            if ([firstLine hasPrefix:@"#!"]) {
                if ([firstLine rangeOfString:@"python"].location != NSNotFound) return @"python";
                if ([firstLine rangeOfString:@"node"].location != NSNotFound) return @"javascript";
                if ([firstLine rangeOfString:@"ruby"].location != NSNotFound) return @"ruby";
                if ([firstLine rangeOfString:@"perl"].location != NSNotFound) return @"perl";
                if ([firstLine rangeOfString:@"bash"].location != NSNotFound ||
                    [firstLine rangeOfString:@"/sh"].location != NSNotFound) return @"bash";
                if ([firstLine rangeOfString:@"lua"].location != NSNotFound) return @"lua";
                if ([firstLine rangeOfString:@"php"].location != NSNotFound) return @"php";
            }
        }
    }

    return nil;
}

// ============================================================================
// UNClient Class - Main SDK Interface
// ============================================================================

/**
 * UNClient - Unsandbox API client with stored credentials.
 *
 * Example usage:
 *   UNClient *client = [[UNClient alloc] init];
 *   NSDictionary *result = [client execute:@"python" code:@"print('Hello')"];
 *   NSLog(@"Output: %@", result[@"stdout"]);
 *
 *   // Or with explicit credentials:
 *   UNClient *client = [[UNClient alloc] initWithPublicKey:@"unsb-pk-..." secretKey:@"unsb-sk-..."];
 */
@interface UNClient : NSObject

@property (nonatomic, strong, readonly) NSString* publicKey;
@property (nonatomic, strong, readonly) NSString* secretKey;

/**
 * Initialize client with automatic credential loading.
 * Loads from environment variables or ~/.unsandbox/accounts.csv
 */
- (instancetype)init;

/**
 * Initialize client with explicit credentials.
 *
 * @param publicKey API public key (unsb-pk-...)
 * @param secretKey API secret key (unsb-sk-...)
 */
- (instancetype)initWithPublicKey:(NSString*)publicKey secretKey:(NSString*)secretKey;

/**
 * Execute code synchronously.
 *
 * @param language Programming language (python, javascript, go, rust, etc.)
 * @param code Source code to execute
 * @return Dictionary with stdout, stderr, exit_code, job_id
 */
- (NSDictionary*)execute:(NSString*)language code:(NSString*)code;

/**
 * Execute code with options.
 *
 * @param language Programming language
 * @param code Source code
 * @param options Dictionary with optional keys: env, input_files, network_mode, ttl, vcpu, return_artifact
 * @return Dictionary with stdout, stderr, exit_code, job_id
 */
- (NSDictionary*)execute:(NSString*)language code:(NSString*)code options:(NSDictionary*)options;

/**
 * Execute code asynchronously. Returns immediately with job_id.
 *
 * @param language Programming language
 * @param code Source code
 * @param options Optional execution options
 * @return Dictionary with job_id, status ("pending")
 */
- (NSDictionary*)executeAsync:(NSString*)language code:(NSString*)code options:(NSDictionary*)options;

/**
 * Execute code with automatic language detection from shebang.
 *
 * @param code Source code with shebang (e.g., #!/usr/bin/env python3)
 * @return Dictionary with detected_language, stdout, stderr, etc.
 */
- (NSDictionary*)run:(NSString*)code;

/**
 * Execute with auto-detect, asynchronously.
 *
 * @param code Source code with shebang
 * @return Dictionary with job_id, detected_language, status
 */
- (NSDictionary*)runAsync:(NSString*)code;

/**
 * Get job status and results.
 *
 * @param jobId Job ID from executeAsync or runAsync
 * @return Dictionary with job_id, status, result (if completed)
 */
- (NSDictionary*)getJob:(NSString*)jobId;

/**
 * Wait for job completion with exponential backoff polling.
 *
 * @param jobId Job ID to wait for
 * @return Final job result dictionary
 */
- (NSDictionary*)wait:(NSString*)jobId;

/**
 * Wait for job with max polls limit.
 *
 * @param jobId Job ID to wait for
 * @param maxPolls Maximum number of poll attempts
 * @return Final job result dictionary
 */
- (NSDictionary*)wait:(NSString*)jobId maxPolls:(int)maxPolls;

/**
 * Cancel a running job.
 *
 * @param jobId Job ID to cancel
 * @return Dictionary with partial output collected before cancellation
 */
- (NSDictionary*)cancelJob:(NSString*)jobId;

/**
 * List all active jobs for this API key.
 *
 * @return Array of job summary dictionaries
 */
- (NSArray*)listJobs;

/**
 * Generate images from text prompt.
 *
 * @param prompt Text description of the image to generate
 * @return Dictionary with images array, created_at
 */
- (NSDictionary*)image:(NSString*)prompt;

/**
 * Generate images with options.
 *
 * @param prompt Text prompt
 * @param options Dictionary with optional keys: model, size, quality, n
 * @return Dictionary with images array
 */
- (NSDictionary*)image:(NSString*)prompt options:(NSDictionary*)options;

/**
 * Get list of supported programming languages.
 * Results are cached in ~/.unsandbox/languages.json for 1 hour.
 *
 * @return Dictionary with languages array, count, aliases
 */
- (NSDictionary*)languages;

/**
 * Make authenticated API request.
 *
 * @param endpoint API endpoint (e.g., /execute)
 * @param method HTTP method
 * @param data Request body dictionary (or nil)
 * @return Response dictionary
 */
- (NSDictionary*)apiRequest:(NSString*)endpoint method:(NSString*)method data:(NSDictionary*)data;

/**
 * Make API request with text/plain body.
 */
- (NSDictionary*)apiRequestText:(NSString*)endpoint method:(NSString*)method body:(NSString*)body;

@end

@implementation UNClient

- (instancetype)init {
    self = [super init];
    if (self) {
        NSString* pk = nil;
        NSString* sk = nil;
        NSError* error = nil;
        if (!UNGetCredentials(&pk, &sk, nil, nil, &error)) {
            @throw [NSException exceptionWithName:@"UNAuthenticationError"
                                           reason:[error localizedDescription]
                                         userInfo:nil];
        }
        _publicKey = pk;
        _secretKey = sk;
    }
    return self;
}

- (instancetype)initWithPublicKey:(NSString*)publicKey secretKey:(NSString*)secretKey {
    self = [super init];
    if (self) {
        _publicKey = publicKey;
        _secretKey = secretKey;
    }
    return self;
}

- (NSDictionary*)apiRequest:(NSString*)endpoint method:(NSString*)method data:(NSDictionary*)data {
    NSString* urlString = [UN_API_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:method];
    [request setTimeoutInterval:UN_DEFAULT_TIMEOUT];

    // Prepare body
    NSString* bodyString = @"";
    if (data) {
        NSError* error = nil;
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:data options:0 error:&error];
        if (error) {
            return @{@"error": [error localizedDescription]};
        }
        bodyString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [request setHTTPBody:jsonData];
    }

    // Generate timestamp and signature
    long timestamp = (long)[[NSDate date] timeIntervalSince1970];
    NSString* signature = UNComputeSignature(_secretKey, timestamp, method, endpoint, bodyString);

    // Set headers
    [request setValue:[@"Bearer " stringByAppendingString:_publicKey] forHTTPHeaderField:@"Authorization"];
    [request setValue:[NSString stringWithFormat:@"%ld", timestamp] forHTTPHeaderField:@"X-Timestamp"];
    [request setValue:signature forHTTPHeaderField:@"X-Signature"];
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];

    NSHTTPURLResponse* response = nil;
    NSError* error = nil;
    NSData* responseData = [NSURLConnection sendSynchronousRequest:request
                                                 returningResponse:&response
                                                             error:&error];

    if (error) {
        return @{@"error": [error localizedDescription]};
    }

    if ([response statusCode] != 200 && [response statusCode] != 201) {
        NSString* errMsg = responseData ? [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding] : @"Unknown error";
        return @{@"error": errMsg, @"status_code": @([response statusCode])};
    }

    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
    if (error) {
        return @{@"error": [error localizedDescription]};
    }

    return result;
}

- (NSDictionary*)apiRequestText:(NSString*)endpoint method:(NSString*)method body:(NSString*)body {
    NSString* urlString = [UN_API_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:method];
    [request setTimeoutInterval:UN_DEFAULT_TIMEOUT];
    [request setHTTPBody:[body dataUsingEncoding:NSUTF8StringEncoding]];

    long timestamp = (long)[[NSDate date] timeIntervalSince1970];
    NSString* signature = UNComputeSignature(_secretKey, timestamp, method, endpoint, body);

    [request setValue:[@"Bearer " stringByAppendingString:_publicKey] forHTTPHeaderField:@"Authorization"];
    [request setValue:[NSString stringWithFormat:@"%ld", timestamp] forHTTPHeaderField:@"X-Timestamp"];
    [request setValue:signature forHTTPHeaderField:@"X-Signature"];
    [request setValue:@"text/plain" forHTTPHeaderField:@"Content-Type"];

    NSHTTPURLResponse* response = nil;
    NSError* error = nil;
    NSData* responseData = [NSURLConnection sendSynchronousRequest:request
                                                 returningResponse:&response
                                                             error:&error];

    if (error || ([response statusCode] != 200 && [response statusCode] != 201)) {
        return @{@"error": @"Request failed"};
    }

    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
    return result ?: @{};
}

- (NSDictionary*)execute:(NSString*)language code:(NSString*)code {
    return [self execute:language code:code options:nil];
}

- (NSDictionary*)execute:(NSString*)language code:(NSString*)code options:(NSDictionary*)options {
    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"language": language,
        @"code": code,
        @"network_mode": options[@"network_mode"] ?: @"zerotrust",
        @"ttl": options[@"ttl"] ?: @(UN_DEFAULT_TTL),
        @"vcpu": options[@"vcpu"] ?: @1
    }];

    if (options[@"env"]) payload[@"env"] = options[@"env"];
    if (options[@"input_files"]) payload[@"input_files"] = options[@"input_files"];
    if ([options[@"return_artifact"] boolValue]) payload[@"return_artifact"] = @YES;

    return [self apiRequest:@"/execute" method:@"POST" data:payload];
}

- (NSDictionary*)executeAsync:(NSString*)language code:(NSString*)code options:(NSDictionary*)options {
    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"language": language,
        @"code": code,
        @"network_mode": options[@"network_mode"] ?: @"zerotrust",
        @"ttl": options[@"ttl"] ?: @(UN_DEFAULT_TTL),
        @"vcpu": options[@"vcpu"] ?: @1
    }];

    if (options[@"env"]) payload[@"env"] = options[@"env"];
    if (options[@"input_files"]) payload[@"input_files"] = options[@"input_files"];
    if ([options[@"return_artifact"] boolValue]) payload[@"return_artifact"] = @YES;

    return [self apiRequest:@"/execute/async" method:@"POST" data:payload];
}

- (NSDictionary*)run:(NSString*)code {
    NSString* endpoint = [NSString stringWithFormat:@"/run?ttl=%ld&network_mode=zerotrust", (long)UN_DEFAULT_TTL];
    return [self apiRequestText:endpoint method:@"POST" body:code];
}

- (NSDictionary*)runAsync:(NSString*)code {
    NSString* endpoint = [NSString stringWithFormat:@"/run/async?ttl=%ld&network_mode=zerotrust", (long)UN_DEFAULT_TTL];
    return [self apiRequestText:endpoint method:@"POST" body:code];
}

- (NSDictionary*)getJob:(NSString*)jobId {
    NSString* endpoint = [NSString stringWithFormat:@"/jobs/%@", jobId];
    return [self apiRequest:endpoint method:@"GET" data:nil];
}

- (NSDictionary*)wait:(NSString*)jobId {
    return [self wait:jobId maxPolls:100];
}

- (NSDictionary*)wait:(NSString*)jobId maxPolls:(int)maxPolls {
    NSSet* terminalStates = [NSSet setWithArray:@[@"completed", @"failed", @"timeout", @"cancelled"]];

    for (int i = 0; i < maxPolls; i++) {
        int delayIdx = MIN(i, UN_POLL_DELAYS_COUNT - 1);
        usleep(UN_POLL_DELAYS[delayIdx] * 1000);  // Convert ms to microseconds

        NSDictionary* result = [self getJob:jobId];
        NSString* status = result[@"status"];

        if ([terminalStates containsObject:status]) {
            return result;
        }
    }

    return @{@"error": @"Max polls exceeded", @"job_id": jobId};
}

- (NSDictionary*)cancelJob:(NSString*)jobId {
    NSString* endpoint = [NSString stringWithFormat:@"/jobs/%@", jobId];
    return [self apiRequest:endpoint method:@"DELETE" data:nil];
}

- (NSArray*)listJobs {
    NSDictionary* result = [self apiRequest:@"/jobs" method:@"GET" data:nil];
    return result[@"jobs"] ?: @[];
}

- (NSDictionary*)image:(NSString*)prompt {
    return [self image:prompt options:nil];
}

- (NSDictionary*)image:(NSString*)prompt options:(NSDictionary*)options {
    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"prompt": prompt,
        @"size": options[@"size"] ?: @"1024x1024",
        @"quality": options[@"quality"] ?: @"standard",
        @"n": options[@"n"] ?: @1
    }];

    if (options[@"model"]) payload[@"model"] = options[@"model"];

    return [self apiRequest:@"/image" method:@"POST" data:payload];
}

- (NSDictionary*)languages {
    // Check cache first
    if (UNIsCacheValid()) {
        NSDictionary* cached = UNReadLanguagesCache();
        if (cached) {
            return cached;
        }
    }

    // Fetch from API
    NSDictionary* result = [self apiRequest:@"/languages" method:@"GET" data:nil];

    // Cache result (only if successful)
    if (result && !result[@"error"]) {
        UNWriteLanguagesCache(result);
    }

    return result;
}

@end

// ============================================================================
// Standalone Library Functions
// ============================================================================

/**
 * Execute code synchronously (standalone function).
 * Uses credentials from environment or config file.
 */
NSDictionary* UNExecute(NSString* language, NSString* code, NSDictionary* options) {
    UNClient* client = [[UNClient alloc] init];
    return [client execute:language code:code options:options];
}

/**
 * Execute code asynchronously (standalone function).
 */
NSDictionary* UNExecuteAsync(NSString* language, NSString* code, NSDictionary* options) {
    UNClient* client = [[UNClient alloc] init];
    return [client executeAsync:language code:code options:options];
}

/**
 * Execute with auto-detect (standalone function).
 */
NSDictionary* UNRun(NSString* code) {
    UNClient* client = [[UNClient alloc] init];
    return [client run:code];
}

/**
 * Execute async with auto-detect (standalone function).
 */
NSDictionary* UNRunAsync(NSString* code) {
    UNClient* client = [[UNClient alloc] init];
    return [client runAsync:code];
}

/**
 * Get job status (standalone function).
 */
NSDictionary* UNGetJob(NSString* jobId) {
    UNClient* client = [[UNClient alloc] init];
    return [client getJob:jobId];
}

/**
 * Wait for job completion (standalone function).
 */
NSDictionary* UNWait(NSString* jobId) {
    UNClient* client = [[UNClient alloc] init];
    return [client wait:jobId];
}

/**
 * Cancel a job (standalone function).
 */
NSDictionary* UNCancelJob(NSString* jobId) {
    UNClient* client = [[UNClient alloc] init];
    return [client cancelJob:jobId];
}

/**
 * List active jobs (standalone function).
 */
NSArray* UNListJobs(void) {
    UNClient* client = [[UNClient alloc] init];
    return [client listJobs];
}

/**
 * Generate image (standalone function).
 */
NSDictionary* UNImage(NSString* prompt, NSDictionary* options) {
    UNClient* client = [[UNClient alloc] init];
    return [client image:prompt options:options];
}

/**
 * Get supported languages (standalone function).
 * Results are cached for 1 hour in ~/.unsandbox/languages.json
 */
NSDictionary* UNLanguages(void) {
    UNClient* client = [[UNClient alloc] init];
    return [client languages];
}

// ============================================================================
// CLI Helper Functions
// ============================================================================

NSDictionary* apiRequestCLI(NSString* endpoint, NSString* method, NSDictionary* data, NSString* publicKey, NSString* secretKey) {
    NSString* urlString = [UN_API_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:method];
    [request setTimeoutInterval:UN_DEFAULT_TIMEOUT];

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

    long timestamp = (long)[[NSDate date] timeIntervalSince1970];
    NSString* signature = UNComputeSignature(secretKey, timestamp, method, endpoint, bodyString);

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
            UNCheckClockDrift(errMsg);
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

NSDictionary* apiRequestPutTextCLI(NSString* endpoint, NSString* content, NSString* publicKey, NSString* secretKey) {
    NSString* urlString = [UN_API_BASE stringByAppendingString:endpoint];
    NSURL* url = [NSURL URLWithString:urlString];
    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:url];
    [request setHTTPMethod:@"PUT"];
    [request setTimeoutInterval:UN_DEFAULT_TIMEOUT];
    [request setHTTPBody:[content dataUsingEncoding:NSUTF8StringEncoding]];

    long timestamp = (long)[[NSDate date] timeIntervalSince1970];
    NSString* signature = UNComputeSignature(secretKey, timestamp, @"PUT", endpoint, content);

    [request setValue:[@"Bearer " stringByAppendingString:publicKey] forHTTPHeaderField:@"Authorization"];
    [request setValue:[NSString stringWithFormat:@"%ld", timestamp] forHTTPHeaderField:@"X-Timestamp"];
    [request setValue:signature forHTTPHeaderField:@"X-Signature"];
    [request setValue:@"text/plain" forHTTPHeaderField:@"Content-Type"];

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
        }
        exit(1);
    }

    NSDictionary* result = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&error];
    return result;
}

NSString* buildEnvContent(NSArray* envVars, NSString* envFile) {
    NSMutableArray* lines = [NSMutableArray array];
    for (NSString* var in envVars) {
        [lines addObject:var];
    }
    if (envFile && [[NSFileManager defaultManager] fileExistsAtPath:envFile]) {
        NSString* fileContent = [NSString stringWithContentsOfFile:envFile encoding:NSUTF8StringEncoding error:nil];
        for (NSString* line in [fileContent componentsSeparatedByString:@"\n"]) {
            NSString* trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            if ([trimmed length] == 0 || [trimmed hasPrefix:@"#"]) continue;
            [lines addObject:line];
        }
    }
    return [lines componentsJoinedByString:@"\n"];
}

// Service vault functions
void serviceEnvStatus(NSString* serviceId, NSString* publicKey, NSString* secretKey) {
    NSString* endpoint = [NSString stringWithFormat:@"/services/%@/env", serviceId];
    NSDictionary* result = apiRequestCLI(endpoint, @"GET", nil, publicKey, secretKey);
    NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
    NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    printf("%s\n", [jsonString UTF8String]);
}

void serviceEnvSet(NSString* serviceId, NSString* content, NSString* publicKey, NSString* secretKey) {
    NSString* endpoint = [NSString stringWithFormat:@"/services/%@/env", serviceId];
    NSDictionary* result = apiRequestPutTextCLI(endpoint, content, publicKey, secretKey);
    NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
    NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    printf("%s\n", [jsonString UTF8String]);
}

void serviceEnvExport(NSString* serviceId, NSString* publicKey, NSString* secretKey) {
    NSString* endpoint = [NSString stringWithFormat:@"/services/%@/env/export", serviceId];
    NSDictionary* result = apiRequestCLI(endpoint, @"POST", nil, publicKey, secretKey);
    if (result[@"content"]) {
        printf("%s", [result[@"content"] UTF8String]);
    }
}

void serviceEnvDelete(NSString* serviceId, NSString* publicKey, NSString* secretKey) {
    NSString* endpoint = [NSString stringWithFormat:@"/services/%@/env", serviceId];
    apiRequestCLI(endpoint, @"DELETE", nil, publicKey, secretKey);
    printf("%sVault deleted for: %s%s\n", [GREEN UTF8String], [serviceId UTF8String], [RESET UTF8String]);
}

// ============================================================================
// CLI Commands
// ============================================================================

void cmdExecute(NSArray* args) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);
    NSString* sourceFile = nil;
    NSMutableDictionary* envVars = [NSMutableDictionary dictionary];
    NSMutableArray* inputFiles = [NSMutableArray array];
    BOOL artifacts = NO;
    NSString* outputDir = @".";
    NSString* network = nil;
    int vcpu = 0;

    for (NSUInteger i = 0; i < [args count]; i++) {
        NSString* arg = args[i];
        if ([arg isEqualToString:@"-e"] && i + 1 < [args count]) {
            NSArray* parts = [args[++i] componentsSeparatedByString:@"="];
            if ([parts count] >= 2) {
                envVars[parts[0]] = [[parts subarrayWithRange:NSMakeRange(1, [parts count] - 1)] componentsJoinedByString:@"="];
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
        } else if (![arg hasPrefix:@"-"]) {
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

    NSError* error = nil;
    NSString* code = [NSString stringWithContentsOfFile:sourceFile encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        fprintf(stderr, "%sError reading file: %s%s\n",
                [RED UTF8String], [[error localizedDescription] UTF8String], [RESET UTF8String]);
        exit(1);
    }

    NSString* language = UNDetectLanguage(sourceFile);
    if (!language) {
        fprintf(stderr, "%sError: Cannot detect language for %s%s\n",
                [RED UTF8String], [sourceFile UTF8String], [RESET UTF8String]);
        exit(1);
    }

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

    if (artifacts) payload[@"return_artifacts"] = @YES;
    if (network) payload[@"network"] = network;
    if (vcpu > 0) payload[@"vcpu"] = @(vcpu);

    NSDictionary* result = apiRequestCLI(@"/execute", @"POST", payload, publicKey, secretKey);

    NSString* stdoutText = result[@"stdout"] ?: @"";
    NSString* stderrText = result[@"stderr"] ?: @"";

    if ([stdoutText length] > 0) {
        printf("%s%s%s", [BLUE UTF8String], [stdoutText UTF8String], [RESET UTF8String]);
    }
    if ([stderrText length] > 0) {
        fprintf(stderr, "%s%s%s", [RED UTF8String], [stderrText UTF8String], [RESET UTF8String]);
    }

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

void cmdSession(NSArray* args) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);
    BOOL listMode = NO;
    NSString* killId = nil;
    NSString* shell = nil;
    NSString* network = nil;
    int vcpu = 0;
    NSMutableArray* inputFiles = [NSMutableArray array];

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
        NSDictionary* result = apiRequestCLI(@"/sessions", @"GET", nil, publicKey, secretKey);
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
        apiRequestCLI(endpoint, @"DELETE", nil, publicKey, secretKey);
        printf("%sSession terminated: %s%s\n", [GREEN UTF8String], [killId UTF8String], [RESET UTF8String]);
        return;
    }

    NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
        @"shell": shell ?: @"bash"
    }];
    if (network) payload[@"network"] = network;
    if (vcpu > 0) payload[@"vcpu"] = @(vcpu);

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
    NSDictionary* result = apiRequestCLI(@"/sessions", @"POST", payload, publicKey, secretKey);
    printf("%sSession created: %s%s\n", [GREEN UTF8String], [result[@"id"] UTF8String], [RESET UTF8String]);
    printf("%s(Interactive sessions require WebSocket - use un2 for full support)%s\n",
           [YELLOW UTF8String], [RESET UTF8String]);
}

void cmdService(NSArray* args) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);
    BOOL listMode = NO;
    NSString* infoId = nil;
    NSString* logsId = nil;
    NSString* sleepId = nil;
    NSString* wakeId = nil;
    NSString* destroyId = nil;
    NSString* name = nil;
    NSString* ports = nil;
    NSString* type = nil;
    NSString* bootstrap = nil;
    NSString* bootstrapFile = nil;
    NSString* network = nil;
    int vcpu = 0;
    NSMutableArray* inputFiles = [NSMutableArray array];
    NSMutableArray* envVars = [NSMutableArray array];
    NSString* envFile = nil;

    // Check for 'env' subcommand first
    if ([args count] >= 1 && [args[0] isEqualToString:@"env"]) {
        if ([args count] < 3) {
            fprintf(stderr, "Usage: un.m service env <status|set|export|delete> <service_id> [options]\n");
            exit(1);
        }
        NSString* envAction = args[1];
        NSString* envTarget = args[2];

        for (NSUInteger i = 3; i < [args count]; i++) {
            NSString* arg = args[i];
            if ([arg isEqualToString:@"-e"] && i + 1 < [args count]) {
                [envVars addObject:args[++i]];
            } else if ([arg isEqualToString:@"--env-file"] && i + 1 < [args count]) {
                envFile = args[++i];
            }
        }

        if ([envAction isEqualToString:@"status"]) {
            serviceEnvStatus(envTarget, publicKey, secretKey);
        } else if ([envAction isEqualToString:@"set"]) {
            NSString* content = buildEnvContent(envVars, envFile);
            if ([content length] == 0) {
                fprintf(stderr, "%sError: No environment variables to set%s\n", [RED UTF8String], [RESET UTF8String]);
                exit(1);
            }
            serviceEnvSet(envTarget, content, publicKey, secretKey);
        } else if ([envAction isEqualToString:@"export"]) {
            serviceEnvExport(envTarget, publicKey, secretKey);
        } else if ([envAction isEqualToString:@"delete"]) {
            serviceEnvDelete(envTarget, publicKey, secretKey);
        } else {
            fprintf(stderr, "%sError: Unknown env action '%s'. Use status, set, export, or delete%s\n",
                    [RED UTF8String], [envAction UTF8String], [RESET UTF8String]);
            exit(1);
        }
        return;
    }

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
        } else if ([arg isEqualToString:@"-e"] && i + 1 < [args count]) {
            [envVars addObject:args[++i]];
        } else if ([arg isEqualToString:@"--env-file"] && i + 1 < [args count]) {
            envFile = args[++i];
        } else if ([arg isEqualToString:@"-n"] && i + 1 < [args count]) {
            network = args[++i];
        } else if ([arg isEqualToString:@"-v"] && i + 1 < [args count]) {
            vcpu = [args[++i] intValue];
        }
    }

    if (listMode) {
        NSDictionary* result = apiRequestCLI(@"/services", @"GET", nil, publicKey, secretKey);
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
        NSDictionary* result = apiRequestCLI(endpoint, @"GET", nil, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (logsId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/logs", logsId];
        NSDictionary* result = apiRequestCLI(endpoint, @"GET", nil, publicKey, secretKey);
        printf("%s", [result[@"logs"] UTF8String]);
        return;
    }

    if (sleepId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/freeze", sleepId];
        apiRequestCLI(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sService frozen: %s%s\n", [GREEN UTF8String], [sleepId UTF8String], [RESET UTF8String]);
        return;
    }

    if (wakeId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@/unfreeze", wakeId];
        apiRequestCLI(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sService unfreezing: %s%s\n", [GREEN UTF8String], [wakeId UTF8String], [RESET UTF8String]);
        return;
    }

    if (destroyId) {
        NSString* endpoint = [NSString stringWithFormat:@"/services/%@", destroyId];
        apiRequestCLI(endpoint, @"DELETE", nil, publicKey, secretKey);
        printf("%sService destroyed: %s%s\n", [GREEN UTF8String], [destroyId UTF8String], [RESET UTF8String]);
        return;
    }

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

        if (type) payload[@"service_type"] = type;
        if (bootstrap) payload[@"bootstrap"] = bootstrap;

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

        NSDictionary* result = apiRequestCLI(@"/services", @"POST", payload, publicKey, secretKey);
        printf("%sService created: %s%s\n", [GREEN UTF8String], [result[@"id"] UTF8String], [RESET UTF8String]);
        printf("Name: %s\n", [result[@"name"] UTF8String]);
        if (result[@"url"]) {
            printf("URL: %s\n", [result[@"url"] UTF8String]);
        }

        NSString* envContent = buildEnvContent(envVars, envFile);
        if ([envContent length] > 0 && result[@"id"]) {
            printf("%sSetting vault for service...%s\n", [YELLOW UTF8String], [RESET UTF8String]);
            serviceEnvSet(result[@"id"], envContent, publicKey, secretKey);
        }
        return;
    }

    fprintf(stderr, "%sError: Specify --name to create a service, or use --list, --info, env, etc.%s\n",
            [RED UTF8String], [RESET UTF8String]);
    exit(1);
}

void cmdKey(NSArray* args) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);

    printf("%sValid%s\n", [GREEN UTF8String], [RESET UTF8String]);
    printf("Public Key: %s\n", [publicKey UTF8String]);
}

void cmdImage(NSArray* args) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);

    BOOL listMode = NO;
    NSString* infoId = nil;
    NSString* deleteId = nil;
    NSString* lockId = nil;
    NSString* unlockId = nil;
    NSString* publishId = nil;
    NSString* sourceType = nil;
    NSString* visibilityId = nil;
    NSString* visibilityMode = nil;
    NSString* spawnId = nil;
    NSString* cloneId = nil;
    NSString* name = nil;
    NSString* ports = nil;

    for (NSUInteger i = 0; i < [args count]; i++) {
        NSString* arg = args[i];
        if ([arg isEqualToString:@"--list"] || [arg isEqualToString:@"-l"]) {
            listMode = YES;
        } else if ([arg isEqualToString:@"--info"] && i + 1 < [args count]) {
            infoId = args[++i];
        } else if ([arg isEqualToString:@"--delete"] && i + 1 < [args count]) {
            deleteId = args[++i];
        } else if ([arg isEqualToString:@"--lock"] && i + 1 < [args count]) {
            lockId = args[++i];
        } else if ([arg isEqualToString:@"--unlock"] && i + 1 < [args count]) {
            unlockId = args[++i];
        } else if ([arg isEqualToString:@"--publish"] && i + 1 < [args count]) {
            publishId = args[++i];
        } else if ([arg isEqualToString:@"--source-type"] && i + 1 < [args count]) {
            sourceType = args[++i];
        } else if ([arg isEqualToString:@"--visibility"] && i + 2 < [args count]) {
            visibilityId = args[++i];
            visibilityMode = args[++i];
        } else if ([arg isEqualToString:@"--spawn"] && i + 1 < [args count]) {
            spawnId = args[++i];
        } else if ([arg isEqualToString:@"--clone"] && i + 1 < [args count]) {
            cloneId = args[++i];
        } else if ([arg isEqualToString:@"--name"] && i + 1 < [args count]) {
            name = args[++i];
        } else if ([arg isEqualToString:@"--ports"] && i + 1 < [args count]) {
            ports = args[++i];
        }
    }

    if (listMode) {
        NSDictionary* result = apiRequestCLI(@"/images", @"GET", nil, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (infoId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@", infoId];
        NSDictionary* result = apiRequestCLI(endpoint, @"GET", nil, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (deleteId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@", deleteId];
        apiRequestCLI(endpoint, @"DELETE", nil, publicKey, secretKey);
        printf("%sImage deleted: %s%s\n", [GREEN UTF8String], [deleteId UTF8String], [RESET UTF8String]);
        return;
    }

    if (lockId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@/lock", lockId];
        apiRequestCLI(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sImage locked: %s%s\n", [GREEN UTF8String], [lockId UTF8String], [RESET UTF8String]);
        return;
    }

    if (unlockId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@/unlock", unlockId];
        apiRequestCLI(endpoint, @"POST", nil, publicKey, secretKey);
        printf("%sImage unlocked: %s%s\n", [GREEN UTF8String], [unlockId UTF8String], [RESET UTF8String]);
        return;
    }

    if (publishId) {
        if (!sourceType) {
            fprintf(stderr, "%sError: --publish requires --source-type (service or snapshot)%s\n",
                    [RED UTF8String], [RESET UTF8String]);
            exit(1);
        }
        NSMutableDictionary* payload = [NSMutableDictionary dictionaryWithDictionary:@{
            @"source_type": sourceType,
            @"source_id": publishId
        }];
        if (name) payload[@"name"] = name;

        NSDictionary* result = apiRequestCLI(@"/images/publish", @"POST", payload, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%sImage published%s\n", [GREEN UTF8String], [RESET UTF8String]);
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (visibilityId && visibilityMode) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@/visibility", visibilityId];
        NSDictionary* payload = @{@"visibility": visibilityMode};
        apiRequestCLI(endpoint, @"POST", payload, publicKey, secretKey);
        printf("%sImage visibility set to %s: %s%s\n", [GREEN UTF8String], [visibilityMode UTF8String], [visibilityId UTF8String], [RESET UTF8String]);
        return;
    }

    if (spawnId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@/spawn", spawnId];
        NSMutableDictionary* payload = [NSMutableDictionary dictionary];
        if (name) payload[@"name"] = name;
        if (ports) {
            NSArray* portStrings = [ports componentsSeparatedByString:@","];
            NSMutableArray* portNumbers = [NSMutableArray array];
            for (NSString* p in portStrings) {
                [portNumbers addObject:@([p intValue])];
            }
            payload[@"ports"] = portNumbers;
        }

        NSDictionary* result = apiRequestCLI(endpoint, @"POST", payload, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%sService spawned from image%s\n", [GREEN UTF8String], [RESET UTF8String]);
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    if (cloneId) {
        NSString* endpoint = [NSString stringWithFormat:@"/images/%@/clone", cloneId];
        NSMutableDictionary* payload = [NSMutableDictionary dictionary];
        if (name) payload[@"name"] = name;

        NSDictionary* result = apiRequestCLI(endpoint, @"POST", payload, publicKey, secretKey);
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:result options:NSJSONWritingPrettyPrinted error:nil];
        NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        printf("%sImage cloned%s\n", [GREEN UTF8String], [RESET UTF8String]);
        printf("%s\n", [jsonString UTF8String]);
        return;
    }

    fprintf(stderr, "%sError: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone%s\n",
            [RED UTF8String], [RESET UTF8String]);
    exit(1);
}

void cmdLanguages(BOOL jsonOutput) {
    NSString* publicKey, *secretKey;
    UNGetApiKeysCLI(&publicKey, &secretKey);

    NSDictionary* result = apiRequestCLI(@"/languages", @"GET", nil, publicKey, secretKey);
    NSArray* languages = result[@"languages"];

    if (jsonOutput) {
        // Output as JSON array
        NSError* error = nil;
        NSData* jsonData = [NSJSONSerialization dataWithJSONObject:languages options:0 error:&error];
        if (!error && jsonData) {
            NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
            printf("%s\n", [jsonString UTF8String]);
        }
    } else {
        // Output one language per line
        for (NSString* lang in languages) {
            printf("%s\n", [lang UTF8String]);
        }
    }
}

void showHelp(void) {
    printf("unsandbox - Execute code in secure sandboxes\n\n");
    printf("Usage:\n");
    printf("  un.m [options] <source_file>\n");
    printf("  un.m session [options]\n");
    printf("  un.m service [options]\n");
    printf("  un.m image [options]\n");
    printf("  un.m languages [--json]\n");
    printf("  un.m key [options]\n\n");
    printf("Execute options:\n");
    printf("  -e KEY=VALUE    Environment variable (multiple allowed)\n");
    printf("  -f FILE         Input file (multiple allowed)\n");
    printf("  -a              Return artifacts\n");
    printf("  -o DIR          Output directory for artifacts\n");
    printf("  -n MODE         Network mode (zerotrust|semitrusted)\n");
    printf("  -v N            vCPU count (1-8)\n\n");
    printf("Session options:\n");
    printf("  --list          List active sessions\n");
    printf("  --kill ID       Terminate session\n");
    printf("  --shell NAME    Shell/REPL (default: bash)\n\n");
    printf("Service options:\n");
    printf("  --list          List services\n");
    printf("  --info ID       Get service details\n");
    printf("  --logs ID       Get service logs\n");
    printf("  --freeze ID     Freeze service\n");
    printf("  --unfreeze ID   Unfreeze service\n");
    printf("  --destroy ID    Destroy service\n");
    printf("  --name NAME     Create service with name\n");
    printf("  --ports PORTS   Comma-separated ports\n");
    printf("  --bootstrap CMD Bootstrap command\n\n");
    printf("Image options:\n");
    printf("  --list, -l          List all images\n");
    printf("  --info ID           Get image details\n");
    printf("  --delete ID         Delete an image\n");
    printf("  --lock ID           Lock image to prevent deletion\n");
    printf("  --unlock ID         Unlock image\n");
    printf("  --publish ID        Publish image from service/snapshot\n");
    printf("  --source-type TYPE  Source type: service or snapshot\n");
    printf("  --visibility ID MODE  Set visibility: private, unlisted, or public\n");
    printf("  --spawn ID          Spawn new service from image\n");
    printf("  --clone ID          Clone an image\n");
    printf("  --name NAME         Name for spawned service or cloned image\n");
    printf("  --ports PORTS       Ports for spawned service\n\n");
    printf("Languages options:\n");
    printf("  --json          Output as JSON array\n\n");
    printf("Library Usage:\n");
    printf("  #import \"un.m\"\n");
    printf("  UNClient *client = [[UNClient alloc] init];\n");
    printf("  NSDictionary *result = [client execute:@\"python\" code:@\"print('Hello')\"];\n");
}

// ============================================================================
// Main Entry Point
// ============================================================================

#ifndef UN_LIBRARY_ONLY

int main(int argc, const char* argv[]) {
    @autoreleasepool {
        if (argc < 2) {
            showHelp();
            return 1;
        }

        NSMutableArray* args = [NSMutableArray array];
        for (int i = 1; i < argc; i++) {
            [args addObject:[NSString stringWithUTF8String:argv[i]]];
        }

        NSString* firstArg = args[0];

        if ([firstArg isEqualToString:@"--help"] || [firstArg isEqualToString:@"-h"]) {
            showHelp();
            return 0;
        }

        if ([firstArg isEqualToString:@"session"]) {
            cmdSession([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else if ([firstArg isEqualToString:@"service"]) {
            cmdService([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else if ([firstArg isEqualToString:@"image"]) {
            cmdImage([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else if ([firstArg isEqualToString:@"languages"]) {
            BOOL jsonOutput = NO;
            for (NSUInteger i = 1; i < [args count]; i++) {
                if ([args[i] isEqualToString:@"--json"]) {
                    jsonOutput = YES;
                }
            }
            cmdLanguages(jsonOutput);
        } else if ([firstArg isEqualToString:@"key"]) {
            cmdKey([args subarrayWithRange:NSMakeRange(1, [args count] - 1)]);
        } else {
            cmdExecute(args);
        }
    }

    return 0;
}

#endif
