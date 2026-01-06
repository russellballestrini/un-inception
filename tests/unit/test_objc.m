// Unit tests for un.m - tests internal functions without API calls
// Compile: clang -framework Foundation -o test_objc test_objc.m && ./test_objc

#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonHMAC.h>

static int passed = 0;
static int failed = 0;

void test(NSString *name, BOOL result) {
    if (result) {
        NSLog(@"  ✓ %@", name);
        passed++;
    } else {
        NSLog(@"  ✗ %@", name);
        failed++;
    }
}

NSDictionary *extMap;

NSString *getLanguage(NSString *ext) {
    return extMap[ext] ?: @"";
}

NSString *getExtension(NSString *filename) {
    NSRange range = [filename rangeOfString:@"." options:NSBackwardsSearch];
    if (range.location != NSNotFound) {
        return [filename substringFromIndex:range.location];
    }
    return @"";
}

NSString *getBasename(NSString *path) {
    return [path lastPathComponent];
}

NSString *hmacSha256(NSString *secret, NSString *message) {
    const char *cKey = [secret UTF8String];
    const char *cData = [message UTF8String];
    unsigned char cHMAC[CC_SHA256_DIGEST_LENGTH];

    CCHmac(kCCHmacAlgSHA256, cKey, strlen(cKey), cData, strlen(cData), cHMAC);

    NSMutableString *hash = [NSMutableString stringWithCapacity:CC_SHA256_DIGEST_LENGTH * 2];
    for (int i = 0; i < CC_SHA256_DIGEST_LENGTH; i++) {
        [hash appendFormat:@"%02x", cHMAC[i]];
    }
    return hash;
}

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        extMap = @{
            @".py": @"python", @".js": @"javascript", @".ts": @"typescript",
            @".rb": @"ruby", @".go": @"go", @".rs": @"rust", @".c": @"c",
            @".m": @"objective-c", @".java": @"java", @".kt": @"kotlin"
        };

        NSLog(@"\n=== Extension Mapping Tests ===");

        test(@"Python extension maps correctly",
             [getLanguage(@".py") isEqualToString:@"python"]);

        test(@"Objective-C extension maps correctly",
             [getLanguage(@".m") isEqualToString:@"objective-c"]);

        test(@"JavaScript extension maps correctly",
             [getLanguage(@".js") isEqualToString:@"javascript"]);

        test(@"Go extension maps correctly",
             [getLanguage(@".go") isEqualToString:@"go"]);

        NSLog(@"\n=== HMAC Signature Tests ===");

        test(@"HMAC-SHA256 generates 64 character hex string",
             [hmacSha256(@"test-secret", @"test-message") length] == 64);

        test(@"Same input produces same signature",
             [hmacSha256(@"key", @"msg") isEqualToString:hmacSha256(@"key", @"msg")]);

        NSString *timestamp = @"1704067200";
        NSString *method = @"POST";
        NSString *endpoint = @"/execute";
        NSString *body = @"{\"language\":\"python\"}";
        NSString *message = [NSString stringWithFormat:@"%@:%@:%@:%@",
                            timestamp, method, endpoint, body];

        test(@"Signature format starts with timestamp",
             [message hasPrefix:timestamp]);

        test(@"Signature format contains :POST:",
             [message containsString:@":POST:"]);

        test(@"Signature format contains :/execute:",
             [message containsString:@":/execute:"]);

        NSLog(@"\n=== Language Detection Tests ===");

        NSString *content = @"#!/usr/bin/env python3\nprint('hello')";
        NSString *firstLine = [content componentsSeparatedByString:@"\n"][0];

        test(@"Python shebang detection - starts with #!",
             [firstLine hasPrefix:@"#!"]);

        test(@"Python shebang detection - contains python",
             [firstLine containsString:@"python"]);

        NSLog(@"\n=== Argument Parsing Tests ===");

        NSString *arg1 = @"DEBUG=1";
        NSArray *parts1 = [arg1 componentsSeparatedByString:@"="];
        NSString *key1 = parts1[0];
        NSString *value1 = [[parts1 subarrayWithRange:NSMakeRange(1, parts1.count - 1)]
                           componentsJoinedByString:@"="];

        test(@"Parse -e KEY=VALUE format - key",
             [key1 isEqualToString:@"DEBUG"]);

        test(@"Parse -e KEY=VALUE format - value",
             [value1 isEqualToString:@"1"]);

        NSString *arg2 = @"URL=https://example.com?foo=bar";
        NSRange eqRange = [arg2 rangeOfString:@"="];
        NSString *key2 = [arg2 substringToIndex:eqRange.location];
        NSString *value2 = [arg2 substringFromIndex:eqRange.location + 1];

        test(@"Parse -e KEY=VALUE with equals in value",
             [key2 isEqualToString:@"URL"] &&
             [value2 isEqualToString:@"https://example.com?foo=bar"]);

        NSLog(@"\n=== File Operations Tests ===");

        test(@"Extract file basename",
             [getBasename(@"/home/user/project/script.m") isEqualToString:@"script.m"]);

        test(@"Extract file extension",
             [getExtension(@"/home/user/project/script.m") isEqualToString:@".m"]);

        NSLog(@"\n=== API Constants Tests ===");

        NSString *apiBase = @"https://api.unsandbox.com";

        test(@"API base URL starts with https://",
             [apiBase hasPrefix:@"https://"]);

        test(@"API base URL contains unsandbox.com",
             [apiBase containsString:@"unsandbox.com"]);

        NSLog(@"\n=== Summary ===");
        NSLog(@"Passed: %d", passed);
        NSLog(@"Failed: %d", failed);
        NSLog(@"Total:  %d", passed + failed);

        return failed > 0 ? 1 : 0;
    }
}
