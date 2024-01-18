!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2007-01-09
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BYTE is not permitted as a type specifier
!*
!*  REFERENCE                  : Defect 330035 (ACE Feature Number: 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : byte
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  BYTE is an IBM extension which allows integers, logicals, and character*1
!*  elements to be manipulated interchangeably.  Although BYTE arrays are legal,
!*  BYTE was never intended to be a type specifier, so we should verify that
!*  the compiler disallows it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acebyte00d

  implicit none

  byte :: bar(3), bempty(0)
  byte, allocatable :: bara(:)
  integer :: i, iarr(2)
  logical :: larr(2)
  character(1) :: carr(2)

  bar =    [byte:: 1,2,3]
  bar =    [byte:: .true.,.false.,.true.]
  bar =    [byte:: 'a', 'b', 'c']
  bar =    [byte:: 1ha, 1hb, 1hc]

  bar =   (/byte:: (i,i=1,3)/)
  bar =   (/byte:: (i==1,i=1,3)/)
  bar =   (/byte:: ('a', i=1,3)/)
  bar =   (/byte:: (1ha, i=1,3)/)

  bara =  (/byte:: 1,2,3/)
  bara =  (/byte:: .true.,.false.,.true./)
  bara =  (/byte:: 'a', 'b', 'c'/)
  bara =  (/byte:: 1ha, 1hb, 1hc/)

  bara =   [byte:: (i,i=1,3)]
  bara =   [byte:: (i==1,i=1,3)]
  bara =   [byte:: ('a', i=1,3)]
  bara =   [byte:: (1ha, i=1,3)]

  iarr =   [integer:: [byte:: 1], (/byte:: (2,i=1,1)/)]
  larr =   [logical:: [byte:: .false.], [byte:: (i==1,i=1,1)]]
  carr =   [character(1):: (/byte:: 'a'/), [byte:: ('b',i=1,1)]]

  call sub([byte:: 1,2,3])
  call sub([byte:: .true.,.false.,.true.])
  call sub([byte:: 'a', 'b', 'c'])
  call sub([byte:: 1ha, 1hb, 1hc])

  call sub([byte:: (i,i=1,3)])
  call sub([byte:: (i==1,i=1,3)])
  call sub([byte:: ('a', i=1,3)])
  call sub([byte:: (1ha, i=1,3)])

  call subi((/integer:: [byte:: 1], [byte:: (2,i=1,1)]/))
  call subl([logical:: (/byte:: .false./), [byte:: (i==1,i=1,1)]])
  call subc([character(1):: [byte:: 'a'], (/byte:: ('b',i=1,1)/)])

  print *,(/byte:: 1,2,3/)
  print *,(/byte:: .true.,.false.,.true./)
  print *,(/byte:: 'a', 'b', 'c'/)
  print *,(/byte:: 1ha, 1hb, 1hc/)

  print *, [byte:: (i,i=1,3)]
  print *, [byte:: (i==1,i=1,3)]
  print *, [byte:: ('a', i=1,3)]
  print *, [byte:: (1ha, i=1,3)]

  print *, [integer:: [byte:: 1], [byte:: (2,i=1,1)]]
  print *, [logical:: [byte:: .false.], [byte:: (i==1,i=1,1)]]
  print *, [character(1):: [byte:: 'a'], [byte:: ('b',i=1,1)]]

  bempty = []
  bempty = [byte::]
  bempty =(/byte:: byte::/)

contains

  subroutine sub(arg)
    byte :: arg(:)
  end subroutine sub

  subroutine subi(arg)
    integer :: arg(:)
  end subroutine subi

  subroutine subl(arg)
    logical :: arg(:)
  end subroutine subl

  subroutine subc(arg)
    character(1) :: arg(:)
  end subroutine subc

end program acebyte00d
