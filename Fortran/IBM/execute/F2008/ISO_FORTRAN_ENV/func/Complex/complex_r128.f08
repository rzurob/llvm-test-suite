  program complex_r32
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   implicit none

   REAL(REAL128) :: Z
   complex(kind=KIND(Z)) :: X
   complex(kind=REAL128) :: Y

     X=(3838838,2349_4)
     Y=X

     print*,"X=",X
     print*,"Y=",Y
     print*,"ADD=",Y+X
     print*,"SUB=",Y-X
     print*,"MUL=",Y*X
     print*,"DIV=",Y/X

  end program
