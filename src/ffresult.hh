#pragma once
#include "fferror.hh"

namespace ff {
template <typename T>
struct Result {
    Error error_{0};

    [[nodiscard]] Result(T &&value) : value_(value) {}
    [[nodiscard]] Result(Error error) : error_(error) {}
    [[nodiscard]] inline bool ok() { return error_ == Error::Ok; }
    [[nodiscard]] inline T &value() {
        if (not ok()) throw BadResultDereference();
        return value_;
    }

    [[nodiscard]] inline operator bool() { return ok(); }
    [[nodiscard]] inline operator T &() { return value(); }

   private:
    T value_;
};

template <>
struct Result<void> {
    Error error_{0};

    [[nodiscard]] Result() {}
    [[nodiscard]] Result(Error error) : error_(error) {}
    [[nodiscard]] inline bool ok() { return error_ == Error::Ok; }
    [[nodiscard]] inline operator bool() { return ok(); }
};
}  // namespace ff
