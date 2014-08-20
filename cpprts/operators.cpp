
#include "boxing.h"
#include "operators.h"

namespace idris {

template <Closure::Type T>
Value general_operator(const Closure::Op op, const Value& lhs, const Value& rhs = nullptr) {
  switch (op) {
    case Closure::Op::Equals:
      return box<Closure::Type::Int>(unbox<T>(lhs) == unbox<T>(rhs));
    case Closure::Op::Less:
      return box<Closure::Type::Int>(unbox<T>(lhs) < unbox<T>(rhs));
    case Closure::Op::LessEquals:
      return box<Closure::Type::Int>(unbox<T>(lhs) <= unbox<T>(rhs));
    case Closure::Op::Concat:
      return box<T>(unbox<T>(lhs) + unbox<T>(rhs));      
    case Closure::Op::ToString: {
      ostringstream strstream;
      strstream << unbox<T>(lhs);
      return box<Closure::Type::String>(strstream.str());
    }
    default:
      RAISE("unsupported operator type",int(op));
      return nullptr;
    }
}

template <Closure::Type T>
Value number_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    // Binary
    case Closure::Op::Plus:
      return box<T>(unbox<T>(lhs) + unbox<T>(rhs));
    case Closure::Op::Minus:
      return box<T>(unbox<T>(lhs) - unbox<T>(rhs));
    case Closure::Op::Star:
      return box<T>(unbox<T>(lhs) * unbox<T>(rhs));
    case Closure::Op::Slash:
      return box<T>(unbox<T>(lhs) / unbox<T>(rhs));
    default:
      return general_operator<T>(op, lhs, rhs);
  }
}

template <Closure::Type T>
Value integral_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {
  switch (op) {
    case Closure::Op::Percent:
      return box<T>(unbox<T>(lhs) % unbox<T>(rhs));
    case Closure::Op::BitAnd:
      return box<T>(unbox<T>(lhs) & unbox<T>(rhs));
    case Closure::Op::BitOr:
      return box<T>(unbox<T>(lhs) | unbox<T>(rhs));
    case Closure::Op::BitXor:
      return box<T>(unbox<T>(lhs) ^ unbox<T>(rhs));
    case Closure::Op::ShiftLeft:
      return box<T>(unbox<T>(lhs) << unbox<T>(rhs));
    case Closure::Op::ShiftRight: {
      return box<T>(unbox<T>(lhs) >> unbox<T>(rhs));
    }
    default:
      return number_operator<T>(op, lhs, rhs);
  }
}

Value to_string(const Value& value) {
  if (value->type == Closure::Type::Word8) { // necessary since template is ambiguous with 'char'
    return general_operator<Closure::Type::Int>(Closure::Op::ToString, value);
  } else {
    return find_type_and_apply_operator(Closure::Op::ToString, value);
  }
}  
  
Value find_type_and_apply_operator(const Closure::Op op, const Value& lhs, const Value& rhs) {

  switch (lhs->type) {

    case Closure::Type::Int:
      return integral_operator<Closure::Type::Int>(op, lhs, rhs);
    case Closure::Type::Float:
      return number_operator<Closure::Type::Float>(op, lhs, rhs);
    case Closure::Type::String:
      return general_operator<Closure::Type::String>(op, lhs, rhs);
    case Closure::Type::Char:
      return general_operator<Closure::Type::Char>(op, lhs, rhs);
    case Closure::Type::BigInt:
      return integral_operator<Closure::Type::BigInt>(op, lhs, rhs);

    case Closure::Type::Word8:
      return integral_operator<Closure::Type::Word8>(op,lhs, rhs);
    case Closure::Type::Word16:
      return integral_operator<Closure::Type::Word16>(op,lhs, rhs);
    case Closure::Type::Word32:
      return integral_operator<Closure::Type::Word32>(op,lhs, rhs);
    case Closure::Type::Word64:
      return integral_operator<Closure::Type::Word64>(op,lhs, rhs);

    default:
      RAISE("unsupported operand type",int(lhs->type));
      return nullptr;
  }
}

} // namespace idris

