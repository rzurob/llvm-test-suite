!*  ===================================================================
!*
!*  DATE                       : 2/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with scalar as its arguemnt
!*                               Using DIM and MASK with different kind of
!*                               data type.
!* ===================================================================
@process intlog

program mxminlocConform03

    integer v1(2,3), v5(6), v6(2), v7(3)
    logical v4(10)
    byte , dimension(2,3) ::  v3 =reshape((/1,0,1,0,1,0/), (/2,3/))

    v1 = 45
    v5 = 33
    v4 = .true.

    if(any(maxloc((/(char(i+70), i = 1,10,1)/)) .ne. 10)) error stop 1_4

    if(maxloc((/(char(i+70), i = 1,10,1)/), dim=1, mask=.true.) .ne. 10) error stop 2_4

    if(maxloc((/(char(i+70), i = 1,10,1)/), dim=b"001", mask=v4) .ne. 10) error stop 3_4

    v6 =  minloc(reshape((/"bb", "aa", "cc", "dd","ff", "gg"/),(/2,3/)), mask=.true.)

    if(v6(1) .ne. 2 .or. v6(2) .ne. 1) error stop 4_4

    v6 = maxloc(reshape((/"bb", "aa", "cc", "dd","ff", "gg"/),(/2,3/)), mask= v3)

    if(v6(1) .ne. 1 .or. v6(2) .ne. 3) error stop 5_4

    v7 = maxloc(reshape((/"bb", "aa", "cc", "dd","ff", "gg"/),(/2,3/)), dim=b"001", mask=b"001")

    if(v7(1) .ne. 1 .or. v7(2) .ne. 2 .or. v7(3) .ne. 2) error stop 6_4

    v7 = minloc(reshape((/"bb", "aa", "cc", "dd","ff", "gg"/),(/2,3/)), dim=b"001", mask= v1)

    if(v7(1) .ne. 2 .or. v7(2) .ne. 1 .or. v7(3) .ne. 1) error stop 7_4

end program mxminlocConform03

