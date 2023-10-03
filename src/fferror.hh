#pragma once

#include <exception>

namespace ff {
struct BadResultDereference : std::exception {
    const char *what() { return "Tried dereferencing result when an error occured."; }
};
enum class Error {
    Ok = 0,
    FtInitializationFail,
    FtDecomposeFail,
    FtLoadCharFail,
    ShaderLinkageFail,
    MsdfVertexShaderCompileFail,
    MsdfFragmentShaderCompileFail,
    FontVertexShaderCompileFail,
    FontFragmentShaderCompileFail,
    FontGeometryShaderCompileFail,
    ShaderProgramCreationFail,
    FtFaceInitializationFail,
    ExceededMaxTextureSize,
    OutOfGpuMemory,
    GlyphGenerationFail,
    BadIndexMapAccess,
};

}  // namespace ff
