!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as actual
!*                               argument to subprogram with generic
!*                               interface. Interface name is the same
!*                               as its intrinsic name.
!* ===================================================================

   program mxminlocVarGenInterface

       intrinsic maxloc

       interface maxloc
          integer function mxmn_int(arg1, arg2)
                integer arg1, arg2
          end function
       end interface

       integer v(2)
       character*2 :: x(2,3)= reshape((/"aa","bb","ff","dd","ee","ff"/), (/2,3/))

       v =  maxloc(x)

       if(v(1) .ne. 1 .or. v(2) .ne. 2) error stop 1_4

       v = maxloc(x, dim=2, mask=.true.)

       if(v(1) .ne. 2 .or. v(2) .ne. 3) error stop 2_4

       if(maxloc(3, 4) .ne. 7) error stop 3_4

   end program mxminlocVarGenInterface

      integer function mxmn_int(arg1, arg2)
            integer arg1, arg2
            mxmn_int = arg1 + arg2
      end function

