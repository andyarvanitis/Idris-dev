
#include "boxing.h"
#include "operators.h"

namespace idris {

Value find_type_and_apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {

  switch (lhs->type) {

    case Closure::Type::Int:
      return integral_operator<int>(op, lhs, rhs);
    case Closure::Type::Float:
      return number_operator<double>(op, lhs, rhs);
    case Closure::Type::String:
      return general_operator<string>(op, lhs, rhs);
    case Closure::Type::Char:
      return general_operator<char32_t>(op, lhs, rhs);
    case Closure::Type::BigInt:
      return integral_operator<long long int>(op, lhs, rhs);

    case Closure::Type::Word8:
      return integral_operator<uint8_t>(op,lhs, rhs);
    case Closure::Type::Word16:
      return integral_operator<uint16_t>(op,lhs, rhs);
    case Closure::Type::Word32:
      return integral_operator<uint32_t>(op,lhs, rhs);
    case Closure::Type::Word64:
      return integral_operator<uint64_t>(op,lhs, rhs);

    default:
      RAISE("unsupported operand type",int(lhs->type));
      return nullptr;
  }
}

template <typename T>
Value general_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    case Closure::Op::Equals:
      return box<int>(unbox<T>(lhs) == unbox<T>(rhs));
    case Closure::Op::Less:
      return box<int>(unbox<T>(lhs) < unbox<T>(rhs));
    case Closure::Op::LessEquals:
      return box<int>(unbox<T>(lhs) <= unbox<T>(rhs));
    case Closure::Op::Concat:
      return box(unbox<T>(lhs) + unbox<T>(rhs));      
    case Closure::Op::ToString: {
      ostringstream strstream;
      strstream << unbox<T>(lhs);
      return box(strstream.str());
    }
    default:
      RAISE("unsupported operator type",int(op));
      return nullptr;
    }
}

template <typename T>
Value number_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    // Binary
    case Closure::Op::Plus:
      return box(unbox<T>(lhs) + unbox<T>(rhs));
    case Closure::Op::Minus:
      return box(unbox<T>(lhs) - unbox<T>(rhs));
    case Closure::Op::Star:
      return box(unbox<T>(lhs) * unbox<T>(rhs));
    case Closure::Op::Slash:
      return box(unbox<T>(lhs) / unbox<T>(rhs));
    default:
      return general_operator<T>(op, lhs, rhs);
  }
}

template <typename T>
Value integral_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    case Closure::Op::Percent:
      return box(unbox<T>(lhs) % unbox<T>(rhs));
    case Closure::Op::BitAnd:
      return box(unbox<T>(lhs) & unbox<T>(rhs));
    case Closure::Op::BitOr:
      return box(unbox<T>(lhs) | unbox<T>(rhs));
    case Closure::Op::BitXor:
      return box(unbox<T>(lhs) ^ unbox<T>(rhs));
    case Closure::Op::ShiftLeft:
      return box(unbox<T>(lhs) << unbox<T>(rhs));
    case Closure::Op::ShiftRight: {
      return box(unbox<T>(lhs) >> unbox<T>(rhs));
    }
    default:
      return number_operator<T>(op, lhs, rhs);
  }
}

Value to_string(const Value& value) {
  if (value->type == Closure::Type::Word8) { // necessary since template is ambiguous with 'char'
    return general_operator<int>(Closure::Op::ToString, value);
  } else {
    return find_type_and_apply_operator(Closure::Op::ToString, value);
  }
}  
  
  
} // namespace idris

