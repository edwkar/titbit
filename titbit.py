import sys
import subprocess


SCHEME = 'plt-r5rs'


def main():
  print PRELUDE
  p = subprocess.check_output('%s main.scm' % SCHEME, shell=True)
  print p
  print POSTLUDE



PRELUDE = """
import java.math.BigInteger;


final class SchemeProgram {
  private SchemeProgram() {}

  public static void main(String[] args) {
    //scheme_main();
    System.out.println(
      proc_2(new proc_2_closure(
        BUILTIN_SUBTRACT2,
        BUILTIN_MUL2,
        BUILTIN_IS_ZERO
      ))
    );
  }

  private static class SchemeObject {
    BigInteger intValue() { throw new RuntimeException(); }
    SchemeObject apply() { throw new RuntimeException(); }
    SchemeObject apply(SchemeObject a) { throw new RuntimeException(); }
    SchemeObject apply(SchemeObject a, SchemeObject b) { throw new RuntimeException(); }
    SchemeObject apply(SchemeObject a, SchemeObject b, SchemeObject c) { throw new RuntimeException(); }
    boolean isTrue() { return true; }
  }

  static final class SchemeInt extends SchemeObject {
    final BigInteger value;
    SchemeInt(BigInteger value) { this.value = value; }
    SchemeInt(long value) { this(BigInteger.valueOf(value)); }
    @Override final BigInteger intValue() { return value; }
    @Override public String toString() { return value.toString(); }
    @Override public boolean equals(Object other) { 
      return 
        other instanceof SchemeInt && 
        ((SchemeInt) other).value.equals(this.value);
    }
  }

  static final class SchemePair extends SchemeObject {
    final SchemeObject car;
    final SchemeObject cdr;
    SchemePair(SchemeObject car, SchemeObject cdr) { 
      this.car = car; 
      this.cdr = cdr; 
    }
  }

  static final SchemeObject VALUE_NIL = new SchemeObject() {};
  static final SchemeObject VALUE_TRUE = new SchemeObject() {};
  static final SchemeObject VALUE_FALSE = new SchemeObject() {
    @Override boolean isTrue() { return false; }
  };

  static final SchemeObject BUILTIN_ADD2 = new SchemeObject() {
    @Override final SchemeObject apply(SchemeObject a, SchemeObject b) { 
      return new SchemeInt(a.intValue().add(b.intValue())); 
    }
  };

  static final SchemeObject BUILTIN_SUBTRACT2 = new SchemeObject() {
    @Override final SchemeObject apply(SchemeObject a, SchemeObject b) { 
      return new SchemeInt(a.intValue().subtract(b.intValue())); 
    }
  };

  static final SchemeObject BUILTIN_MUL2 = new SchemeObject() {
    @Override final SchemeObject apply(SchemeObject a, SchemeObject b) { 
      return new SchemeInt(a.intValue().multiply(b.intValue())); 
    }
  };

  static final SchemeObject BUILTIN_IS_ZERO = new SchemeObject() {
    @Override final SchemeObject apply(SchemeObject a) { 
      return a.equals(new SchemeInt(0)) ? VALUE_TRUE : VALUE_FALSE;
    }
  };

  static final SchemeObject BUILTIN_CAR = new SchemeObject() {
    @Override SchemeObject apply(SchemeObject a) { 
      try {
        return ((SchemePair) a).car;
      } catch (ClassCastException e) {
        throw new RuntimeException("not a pair! (" + a + ")");
      }
    }
  };

  static final SchemeObject BUILTIN_CDR = new SchemeObject() {
    @Override SchemeObject apply(SchemeObject a) { 
      try {
        return ((SchemePair) a).cdr;
      } catch (ClassCastException e) {
        throw new RuntimeException("not a pair! (" + a + ")");
      }
    }
  };

  static final SchemeObject BUILTIN_CALL_CC = new SchemeObject() {
    @Override SchemeObject apply(SchemeObject kont) { 
      return kont;
    }
  };
"""


POSTLUDE = """
}
"""


main()
