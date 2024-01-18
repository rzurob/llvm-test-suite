!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : assign CHARACTER noncoarray variables to coarray variables (scalar and array) in main program and vice-versa
!*  ADAPTED FROM               : csSimpleInteger (<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values to character coarray scalars and arrays of different kinds
!*  in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csSimpleCharacter

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~', hash = '###'

    character(1) :: v1, ctmp1, catmp1(10)
    character(3) :: v3, ctmp3, catmp3(10)

    character(1), save :: c1[*] = '', ca1(10)[*] = ''
    character(3), save :: c3[*] = '', ca3(10)[*] = ''

    ! Try to avoid optimisations (v? are quasi-constant: since we don't run tests
    ! which provide command-line arguments, they will always be assigned the values
    ! below, but the optimiser can't know that).
    v1 = hash
    v3 = hash
    if (command_argument_count() < 10) then
      v1 = a1
      v3 = a3
    end if

    ! start with the a value for each len:
    c1 = v1
    c3 = v3

    ca1        = [' ','!','@','$','0','5','9','A','z',v1]
    ca3(1:9:2) = v3 ! odd elements

    if (c1/=v1 .or. c3/=v3) error stop 2
    if (any(ca1 /= [' ','!','@','$','0','5','9','A','z',v1])) then
       print '("ca1:",9(a1,"/"),a1,";")', ca1
       print '("[ ]:",9(a1,"/"),a1,";")', [' ','!','@','$','0','5','9','A','z',v1]
       print *, ca1 /= [' ','!','@','$','0','5','9','A','z',v1]
       print *, any(ca1 /= [' ','!','@','$','0','5','9','A','z',v1])
       error stop 3
    end if
    if (any(ca3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])) then
       print '("ca3:",9(a3,"/"),a3,";")', ca3
       print '("[ ]:",9(a3,"/"),a3,";")', [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
       print *, ca3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
       print *, any(ca3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])
       error stop 4
    end if

    ! now test assignment *from* the coarrays:
    ctmp1  = c1
    ctmp3  = ctmp3
    catmp1 = ca1
    catmp3 = ca3
    if (ctmp1 /= v1 .or. ctmp3 /= v3) error stop 5
    if (any(catmp1 /= [' ','!','@','$','0','5','9','A','z',v1])) then
       print '("catmp1:",9(a1,"/"),a1,";")', catmp1
       print '("   [ ]:",9(a1,"/"),a1,";")', [' ','!','@','$','0','5','9','A','z',v1]
       print *, catmp1 /= [' ','!','@','$','0','5','9','A','z',v1]
       print *, any(catmp1 /= [' ','!','@','$','0','5','9','A','z',v1])
       error stop 6
    end if
    if (any(catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])) then
       print '("catmp3:",9(a3,"/"),a3,";")', catmp3
       print '("   [ ]:",9(a3,"/"),a3,";")', [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
       print *, catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   ']
       print *, any(catmp3 /= [v3,'   ',v3,'   ',v3,'   ',v3,'   ',v3,'   '])
       error stop 7
    end if


    ! now set to b value
    if (command_argument_count() < 10) then
      v1 = b1
      v3 = b3
    end if

    c1 = v1
    c3 = v3

    ca1        = hash
    ca1(1:9:2) = v1 ! odd elements
    ca3        = [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']

    if (c1/=v1 .or. c3/=v3) error stop 12
    if (any(ca1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])) then
       print '("ca1:",9(a1,"/"),a1,";")', ca1
       print '("[ ]:",9(a1,"/"),a1,";")', [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
       print *, ca1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
       print *, any(ca1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])
       error stop 13
    end if
    if (any(ca3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])) then
       print '("ca3:",9(a3,"/"),a3,";")', ca3
       print '("[ ]:",9(a3,"/"),a3,";")', [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
       print *, ca3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
       print *, any(ca3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])
       error stop 14
    end if

    ! now test assignment *from* the coarrays:
    ctmp1  = c1
    ctmp3  = ctmp3
    catmp1 = ca1
    catmp3 = ca3
    if (ctmp1 /= v1 .or. ctmp3 /= v3) error stop 15
    if (any(catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])) then
       print '("catmp1:",9(a1,"/"),a1,";")', catmp1
       print '("   [ ]:",9(a1,"/"),a1,";")', [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
       print *, catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash]
       print *, any(catmp1 /= [character(1):: v1,hash,v1,hash,v1,hash,v1,hash,v1,hash])
       error stop 16
    end if
    if (any(catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])) then
       print '("catmp3:",9(a3,"/"),a3,";")', catmp3
       print '("   [ ]:",9(a3,"/"),a3,";")', [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
       print *, catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~']
       print *, any(catmp3 /= [' !@',v3,'^&*','()_','+AB','CXY','Zab','cdx','yz[','"]~'])
       error stop 17
    end if

end program csSimpleCharacter
