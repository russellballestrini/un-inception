# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name          = 'un'
  spec.version       = '0.1.0'
  spec.authors       = ['Unsandbox']
  spec.email         = ['support@unsandbox.com']

  spec.summary       = 'Ruby SDK for unsandbox.com code execution API'
  spec.description   = 'Synchronous Ruby client for the unsandbox.com secure code execution service. ' \
                       'Execute code in 50+ languages with HMAC-SHA256 authentication.'
  spec.homepage      = 'https://unsandbox.com'
  spec.license       = 'Unlicense'

  spec.required_ruby_version = '>= 2.7.0'

  spec.files         = Dir['src/**/*.rb', 'README.md', 'LICENSE']
  spec.require_paths = ['src']

  spec.metadata = {
    'homepage_uri' => spec.homepage,
    'source_code_uri' => 'https://github.com/unsandbox/un-ruby',
    'documentation_uri' => 'https://unsandbox.com/docs'
  }
end
