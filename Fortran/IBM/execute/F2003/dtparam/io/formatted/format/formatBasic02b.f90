!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. derived type has multiple character scalar and array component.
!*  2. test WRITE statement for dummy argument with assumed length parameter
!*  3. use different edit descriptors
!*  4. output rules:
!* If the specified field width w for an A edit descriptor corresponding to an output item is greater than len, the output field will consist of wlen blanks followed by the len characters from the internal value. If the specified field width w is less than or equal to len, the output field will consist of the leftmost w characters from the internal value.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer,len :: l1,l2
     character(l1) :: c1
     character(l2) :: c2(l1:l2)
     character(l1+l2) :: c3(l1:l1)
  end type

  contains

     subroutine writebase1(arg)
        type(base(*,*)),target,intent(in)  :: arg(:)

        write (10,*) "-----100 : tbase1"
        write (10,100)  arg
        write (10,*) "-----101 : tbase1"
        write (10,101)  arg
        write (10,*) "-----102 : tbase1"
        write (10,102)  arg

        write (10,*) "-----100 : tbase1"
        write (10,100) arg(ubound(arg,1):lbound(arg,1):-1)
        write (10,*) "-----101 : tbase1"
        write (10,101) arg(ubound(arg,1):ubound(arg,1))
        write (10,*) "-----102 : tbase1"
        write (10,102) arg(lbound(arg,1):lbound(arg,1))


        100 format("|",a3,3a5,a8,"|")
        101 format("|",5a10,"|")
        102 format("|",:,a3,:,a3,"|")

     end subroutine

     subroutine writebase2(arg)
        type(base(*,*)),target,intent(in)  :: arg(:)

        write (10,*) "-----103 : tbase2"
        write (10,103)  arg
        write (10,*) "-----104 : tbase2"
        write (10,104)  arg
        write (10,*) "-----105 : tbase2"
        write (10,105)  arg

        103 format("|",tr1,"|",a2,a3,a4,a9,a3,"|",2x,"|",tr3,"|",a2,a3,a4,a9,a3,"|")
        104 format("|",tr6,tl6,a2,a3,2(a2,a5),"|")
        105 format("|",2(a1,a2),(a1,a2),"|",2(a2,a3),"|")

     end subroutine
end module

program formatBasic02b
  use m
  implicit none

  type(base(3,5)),target :: tbase1(2:3)
  type(base(:,:)),target,allocatable :: tbase2(:)
  integer :: ios
  character(256) :: msg

  tbase1=[base(3,5)(c1="xlf",c2=["-123+","-456+","-789+"],c3=["abcde"]), &
          base(3,5)(c1="ON",c2=["000","111","222"],c3=["ibmcanada"])]

  tbase2=[base(3,5)(c1="roger",c2=["intern","cable ","televi"], &
           c3=["xyz"]),tbase1(2)]

  open(unit=10,file='formatBasic02b.out', &
       form='formatted',access='sequential',action='write',iostat=ios)

  if(ios .eq. 0) then

     call writebase1(tbase1)
     call writebase2(tbase2)
  else
     print *,"iostat is not zero, fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 11
  end if

  close(10)

end program
