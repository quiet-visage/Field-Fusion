#pragma once

#include <exception>

namespace msdf {
struct FtInitializationError : std::exception {
    const char *what() { return "FtInitializtionError"; }
};

struct ShaderLinkageError : std::exception {
    const char *what() { return "ShaderLinkageError"; }
};
struct MsdfVertexShaderCompileError : std::exception {
    const char *what() { return "MsdfVertexShaderCompileError"; }
};
struct MsdfFragmentShaderCompileError : std::exception {
    const char *what() { return "MsdfFragmentShaderCompileError"; }
};
struct FontVertexShaderCompileError : std::exception {
    const char *what() { return "FontVertexShaderCompileError"; }
};
struct FontFragmentShaderCompileError : std::exception {
    const char *what() { return "FontFragmentShaderCompileError"; }
};
struct FontGeometryShaderCompileError : std::exception {
    const char *what() { return "FontGeometryShaderCompileError"; }
};
struct FtFaceInitializationError : std::exception {
    const char *what() { return "FtFaceInitializtionError"; }
};
} // namespace msdf
