#ifndef __idris_cpp_box_h_
#define __idris_cpp_box_h_

#include <memory>
#include <vector>
#include <string>
#include <functional>

namespace idris {

using namespace std;  

//---------------------------------------------------------------------------------------

struct BoxedValue {  
  virtual char getTypeId() = 0;
  virtual string asString() = 0;
  virtual long long int asIntegral() = 0;
  virtual ~BoxedValue() {}
};

//---------------------------------------------------------------------------------------

template <char N, typename T>
struct BoxType : public BoxedValue {
  
  using type = T;
  
  char getTypeId() { return N; }
  
  const T value;
  
  // BoxType(T v) : value(v) {}

  template <typename... ArgTypes>
  BoxType(ArgTypes&&... args) : value(forward<ArgTypes>(args)...) {}
  
  inline auto get() -> T { return value; }
  
  string asString();  
  long long int asIntegral();
  
};

//---------------------------------------------------------------------------------------

using Value = shared_ptr<BoxedValue>;

//---------------------------------------------------------------------------------------
// Boxing
//---------------------------------------------------------------------------------------

template <typename T, typename... ArgTypes>
auto box(ArgTypes&&... args) -> Value {
  return make_shared<T>(forward<ArgTypes>(args)...);
}

//---------------------------------------------------------------------------------------
// Unboxing
//---------------------------------------------------------------------------------------

template <typename T>
auto unbox(const Value& boxedValue) -> typename result_of<decltype(&T::get)(T)>::type {
  return dynamic_pointer_cast<T>(boxedValue)->get();
}

//---------------------------------------------------------------------------------------
// Special constructor
//---------------------------------------------------------------------------------------

using IndexType = size_t;
using Func = void (*)(IndexType,IndexType);

struct Constructor {
  
  using Args = vector<Value>;
  
  size_t tag;
  Args args;
  Func function;

  Constructor(const size_t tag, const Args& args = Args(), const Func& function = nullptr) 
    : tag(tag)
    , args(args)
    , function(function)
    {}
};

} // namespace idris


#endif // __idris_cpp_box_h_

