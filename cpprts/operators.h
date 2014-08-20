#ifndef __idris_cpp_runtime_operators_h_
#define __idris_cpp_runtime_operators_h_

#include "types.h"
#include "boxing.h"

namespace idris {

Value find_type_and_apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs = nullptr);

Value to_string(const Value& value);


inline Value operator+(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Plus, lhs, rhs);
}

inline Value operator-(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Minus, lhs, rhs);
}

inline Value operator*(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Star, lhs, rhs);
}

inline Value operator/(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Slash, lhs, rhs);
}

inline Value operator%(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Percent, lhs, rhs);
}

inline Value operator==(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Equals, lhs, rhs);
}

inline Value operator==(const Value& lhs, nullptr_t n) {
  return box<Closure::Type::Int>(not lhs);
}

inline Value operator<(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::Less, lhs, rhs);
}

inline Value operator<=(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::LessEquals, lhs, rhs);
}

inline Value operator&(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitAnd, lhs, rhs);
}

inline Value operator|(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitOr, lhs, rhs);
}

inline Value operator^(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::BitXor, lhs, rhs);
}

inline Value operator<<(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::ShiftLeft, lhs, rhs);
}

inline Value operator>>(const Value& lhs, const Value& rhs) {
  return find_type_and_apply_operator(Closure::Op::ShiftRight, lhs, rhs);
}

} // namespace idris


#endif // __idris_cpp_runtime_operators_h_

