!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Diagnostic TC for maxloc/minloc
!*
!* ===================================================================

  program mxminlocConform01d

     character*4 x1, y1(4), y3(4)
     integer     x2, y2(4)
     logical     z1(2,2)

     parameter(x1 = "abcd")
     parameter(y1 = "dbca")

     parameter(y3 = "sdfg")

     parameter(x2 = 1)
     parameter(y2 = 1)

     parameter(z1 = .true.)

     print *, maxloc(y1, dim = y2)

     print *, maxloc(y1, dim = y2, mask = .true.)

     print *, minloc(x1)

     print *, maxloc(x1, dim=1, mask=.false.)

     print *, minloc(y1, dim=1, mask=z1)

     if(minloc(y1) .ne. 1) then
          error stop 1_4
     endif

 end program mxminlocConform01d