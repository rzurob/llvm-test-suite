!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BGotoNested
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : goto out of a block to an including scope
!*
!*  DESCRIPTION
!*
!*  GOTO to a label in a containing scope should be possible, and not just to an
!*  immediately containing scope, but out of a nested block to the outermost region.
!*  We set up three blocks, one nested within another, and include GOTO's from each
!*  block into each of the containing scopes.  *ALL* GOTO's are legal, but only
!*  one path can be executed at run time to have "success".  Whether or not we
!*  execute a GOTO depends only on the number of command arguments, and none should
!*  be given for a successful run.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BGotoNested
      integer :: tgt
      tgt = command_argument_count() / 10 ! 0 if just "a.out" or anything up to 9 arguments!
      outer: block
        middle: block
          inner: block
            if (tgt >= 2) goto 10
            if (tgt == 1) goto 20
            if (tgt == 0) goto 30
            print *, "inner"
            stop 2
          end block inner
 10       continue
          if (tgt > 2) goto 20
          print *, "outer"
          stop 3
        end block middle
 20     continue
        if (tgt > 1) goto 40
        print *, "outer"
        stop 3
      end block outer
 40   continue
      stop 4
 30   continue
      print *, "success" ! just so we know it worked
end program BGotoNested
