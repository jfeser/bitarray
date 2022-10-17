{ stdenv, fetchFromGithub, cmake, which, m4, bison, flex, python3, llvmPackages
, ... }:
stdenv.mkDerivation rec {
  pname = "ispc";
  version = "1.18.0";

  src = fetchFromGitHub {
    owner = pname;
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-eI/zUhZDTd2SiFltjrs8kxvQQOPHpqhArGyOW+och3E=";
  };

  nativeBuildInputs =
    [ cmake which m4 bison flex python3 llvmPackages.libllvm.dev ];
  buildInputs = with llvmPackages; [ libllvm libclang openmp ncurses ];

  postPatch = ''
    substituteInPlace CMakeLists.txt \
      --replace CURSES_CURSES_LIBRARY CURSES_NCURSES_LIBRARY
  '';

  # the compiler enforces -Werror, and -fno-strict-overflow makes it mad.
  # hilariously this is something of a double negative: 'disable' the
  # 'strictoverflow' hardening protection actually means we *allow* the compiler
  # to do strict overflow optimization. somewhat misleading...
  hardeningDisable = [ "strictoverflow" ];

  cmakeFlags = [
    "-DLLVM_CONFIG_EXECUTABLE=${llvmPackages.llvm.dev}/bin/llvm-config"
    "-DCLANG_EXECUTABLE=${llvmPackages.clang}/bin/clang"
    "-DCLANGPP_EXECUTABLE=${llvmPackages.clang}/bin/clang++"
    "-DISPC_INCLUDE_EXAMPLES=OFF"
    "-DISPC_INCLUDE_UTILS=OFF"
    "-DISPC_MACOS_SDK_PATH=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  ];
}
