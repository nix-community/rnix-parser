{
  http = http://example.com;
  https = https://github.com/NixOS/nixpkgs;
  git = git+ssh://git@github.com/owner/repo.git;
  file = file:///home/user/document.pdf;
  ftp = ftp://ftp.example.com/pub/file.tar.gz;
  
  # URIs with special characters
  withQuery = https://example.com?foo=bar&baz=qux;
  withFragment = https://example.com/page#section;
  withPort = http://localhost:8080/api;
}