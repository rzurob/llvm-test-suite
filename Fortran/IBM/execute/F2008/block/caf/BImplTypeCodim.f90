!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : implicit typing (var appears in CODIMENSION statement)
!*  ADAPTED FROM               : BImplTypePtr
!*
!*  DESCRIPTION
!*
!*  Establish implicit types in the program via an IMPLICIT statement, so that
!*  all variables which are introduced in the program without a type, e.g.,
!*  appearing in any executable statement or any specification statement (like
!*  "CODIMENSION") have a type prescribed by the IMPLICIT statement.
!*  Now introduce variables via a CODIMENSION statement, some identical in name
!*  to variables in the main program, and some unique to the block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BImplTypeCodim

  type :: book(yrWidth,titleLen,nAuthors,authorNameLen)
     integer, kind :: yrWidth = 2
     integer, len  :: titleLen = 50, nAuthors = 1, authorNameLen = 40
     integer(yrWidth) :: year
     character(titleLen) :: title
     character(authorNameLen) :: author(nAuthors)
  end type book

  implicit type(book) (b), integer(4) (i), real(8) (d), character(6) (s)

  ! we declare these initially with a specific type not in accord with the implicit statement
  ! (except for b2)
  type(book) :: i3, b2
  integer(4) :: d4
  real(4) :: b1
  character(4) :: s5

  ! We only need one image to output:
  if (this_image() == 1) then

     b2 = book(1945,'Animal Farm',['Blair, Eric Arthur (aka George Orwell)'])
     i3 = book(1949,'1984',['Blair, Eric Arthur (aka George Orwell)'])
     d4 = 1234567
     b1 = 6.8878e9
     s5 = 'Testing'

     call identifyType('o.b1',b1)
     call identifyType('o.b2',b2)
     call identifyType('o.i3',i3)
     call identifyType('o.d4',d4)
     call identifyType('o.s5',s5)

     block
       codimension :: b1[*], b6[*], i3[*], d4[*], s5[*], s7[*]
       save :: b1, b6, i3, d4, s5, s7

       b1 = book(1968,'2001: A Space Odyssey',['Clarke, Arthur C.'])
       b6 = book(1532,'Il Principe', ['Machiavelli, Niccolo di Bernardo dei'])
       i3 = 7654321
       d4 = 3.14159265358979323_8
       s5 = 'Gnitset'
       s7 = 'extras'
       b2 = book(1851,'Three Billy Goats: Behind the Gruff Exterior', ['Billy Goat, Sr.'])

       call identifyType('i.b1',b1)
       call identifyType('i.b2',b2)
       call identifyType('i.i3',i3)
       call identifyType('i.d4',d4)
       call identifyType('i.s5',s5)
       call identifyType('i.b6',b6)
       call identifyType('i.s7',s7)

     end block

     call identifyType('O.b1',b1)
     call identifyType('O.b2',b2)
     call identifyType('O.i3',i3)
     call identifyType('O.d4',d4)
     call identifyType('O.s5',s5)

  end if

contains

  subroutine identifyType(label,arg)
    character(4) :: label
    class(*) :: arg
    select type(arg)
      type is (integer(2));   print *, label, '.i2', kind(arg), arg
      type is (integer(4));   print *, label, '.i4', kind(arg), arg
      type is (real(4));      print *, label, '.r4', kind(arg), arg
      type is (real(8));      print *, label, '.r8', kind(arg), arg
      type is (character(*)); print *, label, '.c', len(arg), '>', arg, '<'
      class is (book(2,*,*,*)); print *, label, '.b2', arg%yrWidth, arg%year, len(arg%title), '>', &
                                       arg%title, '<', size(arg%author), len(arg%author), '>', (arg%author // '/')
      class is (book(4,*,*,*)); print *, label, '.b4', arg%yrWidth, arg%year, len(arg%title), '>', &
                                       arg%title, '<', size(arg%author), len(arg%author), '>', (arg%author // '/')
      class default;          print *, label, ' unknown'
    end select
  end subroutine

end program BImplTypeCodim
