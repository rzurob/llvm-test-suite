!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic02a.f
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
!*  1. drived type has multiple character scalar and array component.
!*  2. test PRINT statement with function result
!*  3. use different edit descriptor such as repeat count,control edit descriptor like trn,tln,nx and :
!*  4. also test format control reversion
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer,len :: l1,l2
     character(l1) :: c1
     character(l2) :: c2(l1:l2)
     character(l1+l2) :: c3(l1:l1)
  end type

  contains

     function getResult(arg)
        type(base(*,*)),target,intent(in)  :: arg(:)
        type(base(arg%l1,arg%l2)),pointer  :: getResult(:)

        getResult=>arg
     end function
end module

program formatBasic02a
  use m
  implicit none

  type(base(3,5)),pointer :: pbase1(:)=>null()
  type(base(:,:)),pointer :: pbase2(:)=>null()

  type(base(3,5)),target :: tbase1(2:3)
  type(base(:,:)),target,allocatable :: tbase2(:)


  tbase1=[base(3,5)(c1="xlf",c2=["-123+","-456+","-789+"],c3=["abcde"]), &
          base(3,5)(c1="ON",c2=["000","111","222"],c3=["ibmcanada"])]

  tbase2=[base(3,5)(c1="roger",c2=["intern","cable ","televi"], &
           c3=["xyz"]),tbase1(2)]


  pbase1=>tbase1

  pbase2(0:1)=>tbase2

  print *,"-----100 : pbase1"
  print 100, getResult(pbase1)
  print *,"-----101 : tbase1"
  print 101, getResult(tbase1)
  print *,"-----102 : tbase1"
  print 102, tbase1

  print *,"-----100 : pbase1"
  print 100, pbase1(3:2:-1)
  print *,"-----101 : tbase1"
  print 101, getResult(tbase1(3:3))
  print *,"-----102 : pbase1"
  print 102, pbase1(2:2)

  print *,"-----103 : pbase2"
  print 103, getResult(pbase2)
  print *,"-----104 : tbase2"
  print 104, tbase2
  print *,"-----105 : pbase2"
  print 105, pbase2

100 format("|",a3,3a5,a8,"|")
101 format("|",5a10,"|")
102 format("|",:,a3,:,a3,"|")

103 format("|",tr1,"|",a2,a3,a4,a9,a3,"|",2x,"|",tr3,"|",a2,a3,a4,a9,a3,"|")
104 format("|",tr6,tl6,a2,a3,2(a2,a5),"|")
105 format("|",2(a1,a2),(a1,a2),"|",2(a2,a3),"|")

end program
