!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2007
!*
!*  PRIMARY FUNCTIONS TESTED   : -4 return value from SELECTED_REAL_KIND([P, R])
!*                               intrinsic
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the -4 return
!*                               case from selected_real_kind, when embedded
!*                               calls to itself are passed as arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program selectedRealKind5b
		real(8) r
		real(16) f
		integer, dimension(2) :: ans
		parameter (ans = [ -4, -4])
		integer, dimension(2) :: res

	res = [ selected_real_kind (P = selected_real_kind(29,40)+1, R =  selected_real_kind(29,40)*19  ),     &
		selected_real_kind (P = precision(real(0.0,selected_real_kind(P = precision(r)+1, R = range(f)-1)))-1, R = 300 )  ]

         do i = 1, 2
                  if (res(i) .ne. ans(i)) call zzrc(i)
         end do

       end program

