!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) function interface, pass by reference
!*                                        and at C side, we have c function pointer pointing at fortran procedures.
!*                                        - dummy argument and function result with COMPLEX (C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   use ISO_C_BINDING

   interface
      subroutine cset( c1, r1, r2 ) BIND(C)
         import C_DOUBLE_COMPLEX, C_DOUBLE
         COMPLEX(C_DOUBLE_COMPLEX), intent(inout) :: c1
         real(C_DOUBLE), intent(in), value     :: r1
         real(C_DOUBLE), intent(in), value     :: r2
      end subroutine
   end interface

   interface
      COMPLEX(C_FLOAT_COMPLEX) function Cmul(c1) BIND(C)
         import C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
         COMPLEX(C_DOUBLE_COMPLEX), intent(in)        :: c1
      end function
   end interface

   interface
      COMPLEX(C_FLOAT_COMPLEX) function Cadd(c1) BIND(C)
         import C_FLOAT_COMPLEX, C_LONG_DOUBLE_COMPLEX
         COMPLEX(C_LONG_DOUBLE_COMPLEX), intent(in)   :: c1
      end function
   end interface

end module

module m1
   use m

   contains

      COMPLEX(C_FLOAT_COMPLEX) function Fadd(c1) BIND(C)
         COMPLEX(C_LONG_DOUBLE_COMPLEX), intent(in)     :: c1
         print *, "inside Fadd"
         Fadd= c1 + (10.0,20.0);
      end function

      COMPLEX(C_FLOAT_COMPLEX) function Fmul(c1) BIND(C)
         COMPLEX(C_DOUBLE_COMPLEX), intent(in)             :: c1
         print *, "inside Fmul"
         Fmul= 2 * c1;
      end function

end module


program bindc005
   use m1

   COMPLEX(C_DOUBLE_COMPLEX) :: c11
   COMPLEX(C_LONG_DOUBLE_COMPLEX) :: c12 = (10.0, 20.0)
   COMPLEX(C_FLOAT_COMPLEX) :: c21

   procedure(cset), pointer :: setptr => null()
   procedure(cmul), pointer :: mulptr => null()
   procedure(cadd), pointer :: addptr => null()

   setptr => cset
   call setptr( c11, 11.0_C_DOUBLE, 22.0_C_DOUBLE )

   mulptr => Cmul

   c21 = Cmul( c11 )
   print *, c21

   addptr => Cadd

   c21 = addptr(c12)
   print *, c21

end
