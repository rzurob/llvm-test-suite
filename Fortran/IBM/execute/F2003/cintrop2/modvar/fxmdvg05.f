!************************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sep 14,2002
!*
!*  PRIMARY FUNCTIONS TESTED   :test integer(c_long) with bind(c)
!*                              attribute/statement
!*                              test the effect on the binding lables
!*                              with -qmixed option.
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
!*				attribute.
!*                              Verify the result of data passing.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/14/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mod
  use iso_c_binding

  integer(c_long), bind(c,name="xaa") :: fx=1

  integer(c_long):: fy =2
  bind(c,name="Xaa") :: fy

  interface assert_eq2                   ! generic name
    module procedure assert2
  end interface

  contains

    function assert2(n1,n2)
      integer(c_long), intent(in) :: n1,n2
      logical assert2
      if (n1 .ne. n2) then
      assert2= .false.
      else
      assert2=.true.
      endif
      return
    end function assert2

end module

program testmix
  use mod
  logical ::result
  integer(c_long)::basval1,basval2

  basval1 =11
  basval2 =200
  print *, "variables fx and fy are initialized in fortran: fx = ",fx, "fy = ", fy
  call csub()
  result= assert_eq2(fx,basval1)

  if (result .eqv. .false.) then
  error stop 220
  endif


  result= (assert_eq2(fy,basval2))

  if (result .eqv. .false.) then
  error stop 221
  endif
end

