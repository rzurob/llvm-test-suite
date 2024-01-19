!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with constant as actual
!*                               argument to subprogram with defined
!*                               assignment.
!*                               Using substring.
!* ===================================================================

   program mxminlocDefAssign

       interface assignment(=)
          elemental subroutine int_to_char(arg1, arg2)
                integer, intent(in) :: arg2
                character*1, intent(out) :: arg1
          end subroutine
       end interface

       character*20 :: y1(10), y2(2,5)
       character*1  :: x1, x2(2)
       parameter(y1="totallengthistwienty")

       parameter(y2 = "totallengthistwienty")

       x1 =  minloc(y1(1:5)(1:10), dim=1)

       if(x1  .ne. "G") error stop 1_4

       x2 = maxloc(y2, dim=2, mask = .true.)

       if(x2(1) .ne. "G" .or. x2(2) .ne. "G") error stop 2_4

   end program mxminlocDefAssign

   elemental subroutine int_to_char(arg1, arg2)
      integer, intent(in) :: arg2
      character*1, intent(out) :: arg1
      arg1 = char(arg2 + 70 )
   end subroutine

