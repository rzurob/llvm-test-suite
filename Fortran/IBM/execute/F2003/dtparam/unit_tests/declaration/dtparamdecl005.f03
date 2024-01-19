! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TYPE parameters,
!*                                1) kind and len attribute
!*                                2) type parameters used in:
!*                                     i) array high bound
!*                                     ii) derived types
!*                                3) no default values for type param
!*                                4) no inheritance
!*                                5) type parameter values are constants
!*                                6) type parameter values have type parameter keywords
!*                                7) type definition in module
!*                                9) components in derived template have
!*                                     i) allocatable
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module dtparamdecl005m

  type person1
     integer :: pnumber
     integer :: ptype
  end type

  type old(oldkind)
     integer, kind :: oldkind
     integer(oldkind) :: b
  end type

  type base(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     type(person1) :: person1a(baselen)
     type(person1) :: person1b
     type(old(8))  :: old2
     type(old(4)), allocatable :: old3


  end type

  type base2(basekind, baselen)
     integer, kind :: basekind
     integer, len :: baselen
     type(person1) :: person1a(baselen)
     type(person1) :: person1b
     type(old(basekind + 4))  :: old2
     type(old(basekind)), allocatable :: old3


  end type
end module

program dtparamdecl005
  use dtparamdecl005m

  integer :: i
  type(base(4,4)) :: base1, base1a

  type(base2(basekind=4,baselen=4)) :: base2, base2a

  if ((lbound(base1%person1a, 1) .eq. 1) .and. &
      (ubound(base1%person1a, 1) .eq. 4)) then
   print *, ' bounds base1%person1a ok'
  else
   print *, ' bounds base1%person1a failed'
  end if

  do i=1,4
    base1%person1a(i)%pnumber = 50 + i
    base1%person1a(i)%ptype = i
  end do
  print *, ' base1%person1a=', base1%person1a

  base1%person1b%pnumber = 60
  base1%person1b%ptype = 2
  print *, ' base1%person1b=', base1%person1b

  base1%old2%b = 70
  if (kind(base1%old2%b) .eq. 8) then
    print *, ' base1%old2%b kind ok'
  else
    print *, ' base1%old2%b kind failed'
  end if

  allocate(base1%old3)
  if (kind(base1%old3%b) .eq. 4) then
    print *, ' base1%old3%b kind ok'
  else
    print *, ' base1%old3%b kind failed'
  end if
  base1%old3%b = 80

  base1a = base1

  print *, ' base1a%person1a=', base1a%person1a
  print *, ' base1a%person1b=', base1a%person1b
  if (kind(base1a%old2%b) .eq. 8) then
    print *, ' base1a%old2%b kind ok'
  else
    print *, ' base1a%old2%b kind failed'
  end if
  print *, ' base1a%old2%b=', base1a%old2%b
  if (allocated(base1a%old3)) then
    if (kind(base1a%old3%b) .eq. 4) then
      print *, ' base1a%old3%b kind ok'
    else
      print *, ' base1a%old3%b kind failed'
    end if
    print *, ' base1a%old3%b=', base1a%old3%b
  else
    print *, ' error base1a%old3 not allocated'
  end if


  ! ---------------------------------------------------------

  if ((lbound(base2%person1a, 1) .eq. 1) .and. &
      (ubound(base2%person1a, 1) .eq. 4)) then
   print *, ' bounds base2%person1a ok'
  else
   print *, ' bounds base2%person1a failed'
  end if

  do i=1,4
    base2%person1a(i)%pnumber = 50 + i
    base2%person1a(i)%ptype = i
  end do
  print *, ' base2%person1a=', base2%person1a

  base2%person1b%pnumber = 60
  base2%person1b%ptype = 2
  print *, ' base2%person1b=', base2%person1b

  base2%old2%b = 70
  if (kind(base2%old2%b) .eq. 8) then
    print *, ' base2%old2%b kind ok'
  else
    print *, ' base2%old2%b kind failed'
  end if

  allocate(base2%old3)
  if (kind(base2%old3%b) .eq. 4) then
    print *, ' base2%old3%b kind ok'
  else
    print *, ' base2%old3%b kind failed'
  end if
  base2%old3%b = 80

  base2a = base2

  print *, ' base2a%person1a=', base2a%person1a
  print *, ' base2a%person1b=', base2a%person1b
  if (kind(base2a%old2%b) .eq. 8) then
    print *, ' base2a%old2%b kind ok'
  else
    print *, ' base2a%old2%b kind failed'
  end if
  print *, ' base2a%old2%b=', base2a%old2%b
  if (allocated(base2a%old3)) then
    if (kind(base2a%old3%b) .eq. 4) then
      print *, ' base2a%old3%b kind ok'
    else
      print *, ' base2a%old3%b kind failed'
    end if
    print *, ' base2a%old3%b=', base2a%old3%b
  else
    print *, ' error base2a%old3 not allocated'
  end if



end

