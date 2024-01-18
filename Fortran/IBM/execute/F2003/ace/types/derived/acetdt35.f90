!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt35
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-13
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC's in static initialisation, DTIO of AC's
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : static, AC, initialisation, initialization
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Use AC's in static initialisation expressions, then verify values in the
!*  body of the program:  that all intrinsic and derived values are correctly
!*  initialised.  Also, implicitly verify that null() is allowed in initialising
!*  allocatable and pointer components (related diagnostics are in
!*  diag/types/derived/acetdt35d).
!*  At the same time, we test DTIO of initialised arrays and of AC's in the body.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt35mod

  implicit none
  type ADerived
     integer, allocatable :: iafield (:)
     double complex, allocatable :: dzafield
     real, pointer :: rpfield
     character(:), pointer :: cpfield(:)
   contains
     procedure :: printItem => aDerivedPrint
     generic :: write(formatted) => printItem
  end type ADerived

  type Contained
     double precision :: dp
     double complex   :: dz
  end type Contained

  type Derived
     integer :: ifld
     real    :: rfld
     complex :: zfld
     logical :: lfld
     character:: chfld
     type (Contained) :: dtfld
   contains
     procedure :: printItem => derivedPrint
     generic :: write(formatted) => printItem
  end type Derived

contains

  subroutine derivedPrint(this,unit,iotype,vlist,iostat,iomsg)
    class(Derived), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character(100) :: atext, rest

    write(atext,*) this % dtfld
    write(rest,*) this % ifld, this % rfld, this % zfld, this % lfld, this % chfld
    write(unit, "('D[',a,' ',a,']')", iostat=iostat) trim(rest), trim(atext)

  end subroutine derivedPrint


  subroutine aDerivedPrint(this,unit,iotype,vlist,iostat,iomsg)
    class(ADerived), intent(in) :: this
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character(100) :: dctext, itext, rtext, ctext

    if( allocated(this % iafield) ) then
       write(itext,*) this % iafield
    else
       itext = 'null'
    endif

    if( allocated(this % dzafield) ) then
       write(dctext,*) this % dzafield
    else
       dctext = 'null'
    endif

    if( associated(this % rpfield) ) then
       write(rtext,*) this % rpfield
    else
       rtext = 'null'
    endif

    if( associated(this % cpfield) ) then
       write(ctext,*) this % cpfield
    else
       ctext = 'null'
    endif

    write(unit, "('AD[i:',a,',z:',a,',r:',a,',c:',a,']')", iostat=iostat) &
         trim(itext), trim(dctext), trim(rtext), trim(ctext)

  end subroutine aDerivedPrint

end module acetdt35mod


program acetdt35

  use acetdt35mod
  implicit none

  real, target                :: rt1 = 1.1, rta(2) = [real:: 3.4, 5.6]
  character(1), target        :: ct1(3) = [character(1):: 'a', 'b', 'c']
  character(5), target        :: ct5(4) = [character(5):: '>>>th', 'is is', ' a te', 'st<<<']

  integer, parameter          :: iParm = 1
  real, parameter             :: rParm = 2.1
  double precision, parameter :: dParm = atan2(-1d0,1d-9)
  complex, parameter          :: zParm = (1e-9,1e37)
  double complex, parameter   :: dzParm = (1d-40,1d300)
  logical, parameter          :: lParm = .true.
  character, parameter        :: chParm = 'a'
  type (Contained), parameter :: dtParm = Contained(dParm,(sin(dParm)**2,cos(dParm)**2))
  integer :: i, j

  ! These are the types with *no* allocatable/pointer components:
  type (Derived)  :: dArray1(2) = [Derived:: Derived(iParm,rParm,zParm,lParm,chParm,dtParm), &
                                             Derived(32767,2.7182818285,(3.1415926536,1.4142135624),.false.,'b',Contained(dParm,dzParm))]
  type (Derived)  :: dArray2(2) = [Derived:: (Derived(iParm-i,rParm*i,zParm+(real(i),real(-i)),i==1,achar(70+i),Contained(dParm/i,dzParm)), i=1,2)]
  type (Derived)  :: dArray3(2) = [          Derived(iParm,rParm,zParm,lParm,chParm,dtParm), &
                                             Derived(32767,2.7182818285,(3.1415926536,1.4142135624),.false.,'b',Contained(dParm,dzParm))]
  type (Derived)  :: dArray4(2) = [          (Derived(iParm-i,rParm*i,zParm+(real(i),real(-i)),i==1,achar(70+i),Contained(dParm/i,dzParm)), i=1,2)]

  ! These are types *with* allocatable/pointer components:
  type (ADerived) :: adArray1(2) = [ADerived:: ADerived(null(),null(),null(),null()), ADerived(null(),null(),null(),null())]
  type (ADerived) :: adArray2(2) = [ADerived:: (ADerived(null(),null(),null(),null()), i=1,2)]
  type (ADerived) :: adArray3(2) = [ADerived(null(),null(),null(),null()), ADerived(null(),null(),null(),null())]
  type (ADerived) :: adArray4(2) = [(ADerived(null(),null(),null(),null()), i=1,2)]

  ! Show that each parameter has the correct value:
  print *, 'iParm:', iParm
  print *, 'rParm:', rParm
  print *, 'dParm:', dParm
  print *, 'zParm:', zParm
  print *, 'dzParm:', dzParm
  print *, 'lParm:', lParm
  print *, 'chParm:', chParm
  print *, 'dtParm:', dtParm

  print *, 'dArray1:', dArray1
  print *, 'dArray2:', dArray2
  print *, 'dArray3:', dArray3
  print *, 'dArray4:', dArray4

  print *, 'adArray1:', adArray1
  print *, 'adArray2:', adArray2
  print *, 'adArray3:', adArray3
  print *, 'adArray4:', adArray4

  ! Now print AC's of types with no allocatable/pointer components:
  print *, 'AC 1:', [Derived:: Derived(iParm,rParm,zParm,lParm,chParm,dtParm), &
                                             Derived(32767,2.7182818285,(3.1415926536,1.4142135624),.false.,'b',Contained(dParm,dzParm))]
  print *, 'AC 2:', [Derived:: (Derived(iParm-i,rParm*i,zParm+(real(i),real(-i)),i==1,achar(70+i),Contained(dParm/i,dzParm)), i=1,2)]
  print *, 'AC 3:', [          Derived(iParm,rParm,zParm,lParm,chParm,dtParm), &
                                             Derived(32767,2.7182818285,(3.1415926536,1.4142135624),.false.,'b',Contained(dParm,dzParm))]
  print *, 'AC 4:', [          (Derived(iParm-i,rParm*i,zParm+(real(i),real(-i)),i==1,achar(70+i),Contained(dParm/i,dzParm)), i=1,2)]

  ! And types with null allocatable/pointer components:
  print *, 'AAC 1:', [ADerived:: ADerived(null(),null(),null(),null()), ADerived(null(),null(),null(),null())]
  print *, 'AAC 2:', [ADerived:: (ADerived(null(),null(),null(),null()), i=1,2)]
  print *, 'AAC 3:', [ADerived(null(),null(),null(),null()), ADerived(null(),null(),null(),null())]
  print *, 'AAC 4:', [(ADerived(null(),null(),null(),null()), i=1,2)]

  ! [int],(z),r,['']

  ! And finally, the same types with non-null allocatable/pointer components:
  print *, 'AAC 5:', [ADerived:: ADerived([(i,i=1,3)],zParm,rt1,ct1), ADerived([(i*10,i=1,3)],(1.1,3.1),rta(1),ct5)]
  print *, 'AAC 6:', [ADerived:: (ADerived([(i*j,i=1,3*j)],zParm*j,rta(j),ct5), j=1,2)]
  print *, 'AAC 7:', [ADerived([(i,i=1,3)],zParm,rt1,ct1), ADerived([(i*10,i=1,3)],(1.1,3.1),rta(1),ct5)]
  print *, 'AAC 8:', [(ADerived([(i*j,i=1,3*j)],zParm*j,rta(j),ct5), j=1,2)]

end program acetdt35
