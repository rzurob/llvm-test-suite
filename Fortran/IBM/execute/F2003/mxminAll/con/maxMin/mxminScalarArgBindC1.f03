!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as
!*                               argument to BindC
!*
!* ===================================================================

   module mbind1
      interface
         integer(C_INT) function func1(ctx, cty)
            use ISO_C_BINDING
            character(C_CHAR) :: ctx, cty
         end function func1
      end interface
   end module mbind1

  program mxminScalarArgBindC1

   use ISO_C_BINDING
   use mbind1

   character(C_CHAR) :: x, y

   integer ret

   parameter(x = "c")
   parameter(y = "d")

   ret = func1(%ref(min(x, y)), %ref(max(x, y)))

  end program mxminScalarArgBindC1

