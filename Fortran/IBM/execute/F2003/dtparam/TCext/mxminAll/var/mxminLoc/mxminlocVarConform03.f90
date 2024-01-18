! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=self /tstdev/F2003/mxminAll/var/mxminLoc/mxminlocVarConform03.f
! opt variations: -qck -qnok -qnol -qreuse=none

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with derived type component
!*                               as its argument. Using DIM and MASK with 
!*                               different kind of data type. 
!* (315841)
!* ===================================================================
@process intlog

program mxminlocConform03

    type dt1(k1,n1)    ! (4,2)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: x(10) = (/(char(i+70), i = 1,10,1)/)
      character(n1) :: y(2,3) = reshape((/"bb", "aa", "cc", "dd","ff", "gg"/),(/2,3/))
    end type 

    type dt2(n2,k2,k3)    ! (20,4,1)
      integer, kind               :: k2,k3
      integer, len                :: n2
      logical(k2)                 :: v4(6) = .true. 
      integer(k3), dimension(2,3) :: v3 =reshape((/1,0,1,0,1,0/), (/2,3/))
    end type
 
    integer v1(2,3), v5(6), v6(2), v7(3)

    type(dt1(4,2)) :: dt_first
    type(dt2(20,4,1)) :: dt_second

    v1 = 45
    v5 = 33

    if(any(maxloc(dt_first%x(2:5)) .ne.  4)) error stop 1_4

    if(minloc(dt_first%x(3:8), dim=1, mask=dt_second%v4) .ne. 1) error stop 2_4

    if(minloc(dt_first%x(3:8), dim=minloc(dt_first%x,dim=1), mask=v5) .ne. 1) error stop 3_4

    if(any(minloc(dt_first%x(3:8), mask=dt_second%v4 ) .ne. 1)) error stop 4_4

    if(minloc(dt_first%x(3:8), dim=b"001", mask=b"001") .ne. 1) error stop 5_4

    v6 =  minloc(dt_first%y, mask=.true.)
    
    if(v6(1) .ne. 2 .or. v6(2) .ne. 1) error stop 6_4

    v6 = maxloc(dt_first%y, mask= dt_second%v3)

    if(v6(1) .ne. 1 .or. v6(2) .ne. 3) error stop 7_4
    
    v7 = maxloc(dt_first%y, dim=b"001", mask=b"001")
 
    if(v7(1) .ne. 1 .or. v7(2) .ne. 2 .or. v7(3) .ne. 2) error stop 8_4

    v7 = minloc(dt_first%y, dim=b"001", mask= v1)

    if(v7(1) .ne. 2 .or. v7(2) .ne. 1 .or. v7(3) .ne. 1) error stop 9_4

    dt_second%v4(1) = .false.

    if(any(minloc(dt_first%x(3:8), mask=dt_second%v4) .ne. 2)) error stop 10_4

end program mxminlocConform03

