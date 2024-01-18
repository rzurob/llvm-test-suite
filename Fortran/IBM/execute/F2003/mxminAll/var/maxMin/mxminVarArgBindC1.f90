!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with variable as argument
!*                               to BindC 
!*                               
!* ===================================================================

   module mVarbind1
      interface
         integer(C_SIGNED_CHAR) function func1(ctx, cty)
            use ISO_C_BINDING
            character(C_CHAR) :: ctx, cty
         end function func1
      end interface
   end module mVarbind1 
      
  program mxminVarArgBindC1 
    
   use ISO_C_BINDING
   use mVarbind1 

   character(C_CHAR) :: x, y 

   integer ret

   x = "c" 
   y = "d"
 
   ret = func1(%ref(min(x, y)), %ref(max(x, y)))

  end program mxminVarArgBindC1 

