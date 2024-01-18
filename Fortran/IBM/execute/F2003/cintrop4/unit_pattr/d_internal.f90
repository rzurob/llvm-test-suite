!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      !!! Check correctness BIND(C) semantic checking on:
      !!!
      !!!   external subroutine/function/entry
      !!!   module subroutine/function/entry
      !!!   internal subroutine/function/entry
      !!!
      !!! Annotations:
      !!!
      !!! * The F2k standard disallows BIND(C) on internal procedures:
      !!!   source lines marked `illegal' should result in semantic errors.
      !!!
      !!! * Source lines marked `no' are disallowed for other reasons
      !!!   (internal procedures must not contain entry statements)
      !!!   and any BIND(C) error messages aren't detected because the
      !!!   other errors preempt them.
      !!!
      !!! * Source lines marked `ign' are ignored completely
      !!!   (internal procedures must not contain internal procedures)
      !!!   and any BIND(C) error messages aren't displayed.
      !!!
      !!! * Source lines marked `ok' should be accepted without error.
      !!!
      !!! Procedure naming abbreviations:
      !!!   p = program, m = module, s = subroutine, f= function, e = entry

      !! External subroutine and all variants it can contain
      subroutine s () bind(c)  ! ok: global-scope subroutine
      entry se () bind(c)  ! ok: global-scope subroutine-entry
      contains
        subroutine ss () bind(c)  ! illegal: internal subroutine in external sub
        entry sse () bind(c)  ! no: ENTRY stm disallowed in internal procs
        contains  ! no: CONTAINS stm disallowed in internal procs

          !! code after CONTAINS in internal proc ignored
          subroutine sss () bind(c)  ! ign: internal subroutine in internal sub
          entry ssse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          end subroutine
          function ssf() bind(c)  ! ign: internal function in internal sub
            ssf = 1.0
          entry ssfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            ssfe = 1.0
          end function

        end subroutine

        function sf() bind(c)  ! illegal: internal function in external sub
          sf = 1.0
        entry sfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
          sfe = 1.0
        contains  ! no: CONTAINS stm disallowed in internal procs

          !! code after CONTAINS in internal proc ignored
          subroutine sfs () bind(c)  ! ign: internal subroutine in internal sub
          entry sfse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          end subroutine
          function sff() bind(c)  ! ign: internal function in internal sub
            sff = 1.0
          entry sffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            sffe = 1.0
          end function

        end function
      end subroutine

      !! External function and all variants it can contain
      function f() bind(c)  ! ok: global-scope function
        f = 1.0
      entry fe() bind(c)  ! ok: global-scope function-entry
        fe = 1.0
      contains
        subroutine fs () bind(c)  ! illegal: internal subroutine in external fn
        entry fse () bind(c)  ! no: ENTRY stm disallowed in internal procs
        contains  ! no: CONTAINS stm disallowed in internal procs

          !! code after CONTAINS in internal proc ignored
          subroutine fss () bind(c)  ! ign: internal subroutine in internal sub
          entry fsse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          end subroutine
          function fsf() bind(c)  ! ign: internal function in internal sub
            fsf = 1.0
          entry fsfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            fsfe = 1.0
          end function

        end subroutine

        function ff() bind(c)  ! illegal: internal function in external fn
          ff = 1.0
        entry ffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
          ffe = 1.0
        contains  ! no: CONTAINS stm disallowed in internal procs

          !! code after CONTAINS in internal proc ignored
          subroutine ffs () bind(c)  ! ign: internal subroutine in internal sub
          entry ffse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          end subroutine
          function fff() bind(c)  ! ign: internal function in internal sub
            fff = 1.0
          entry fffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            fffe = 1.0
          end function

        end function
      end function

      module m
      contains

        !! Module procedure and all variants it can contain
        subroutine ms () bind(c)  ! ok: module subroutine
        entry mse () bind(c)  ! ok: module subroutine-entry
        contains
          subroutine mss () bind(c)  ! illegal: internal sub in module sub
          entry msse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          contains  ! no: CONTAINS stm disallowed in internal procs

            !! code after CONTAINS in internal proc ignored
            subroutine msss () bind(c)  ! ign: internal sub in internal sub
            entry mssse () bind(c)  ! no: ENTRY stm disallowed in internal procs
            end subroutine
            function mssf() bind(c)  ! ign: internal function in internal sub
              mssf = 1.0
            entry mssfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
              mssfe = 1.0
            end function

          end subroutine

          function msf() bind(c)  ! illegal: internal function in module sub
            msf = 1.0
          entry msfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            msfe = 1.0
          contains  ! no: CONTAINS stm disallowed in internal procs

            !! code after CONTAINS in internal proc ignored
            subroutine msfs () bind(c)  ! ign: internal sub in internal sub
            entry msfse () bind(c)  ! no: ENTRY stm disallowed in internal procs
            end subroutine
            function msff() bind(c)  ! ign: internal function in internal sub
              msff = 1.0
            entry msffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
              msffe = 1.0
            end function

          end function
        end subroutine

        !! Module function and all variants it can contain
        function mf() bind(c)  ! ok: module function
          mf = 1.0
        entry mfe() bind(c)  ! ok: module function-entry
          mfe = 1.0
        contains
          subroutine mfs () bind(c)  ! illegal: internal subroutine in module fn
          entry mfse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          contains  ! no: CONTAINS stm disallowed in internal procs

            !! code after CONTAINS in internal proc ignored
            subroutine mfss () bind(c)  ! ign: internal sub in internal sub
            entry mfsse () bind(c)  ! no: ENTRY stm disallowed in internal procs
            end subroutine
            function mfsf() bind(c)  ! ign: internal function in internal sub
              mfsf = 1.0
            entry mfsfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
              mfsfe = 1.0
            end function

          end subroutine

          function mff() bind(c)  ! illegal: internal function in module fn
            mff = 1.0
          entry mffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            mffe = 1.0
          contains  ! no: CONTAINS stm disallowed in internal procs

            !! code after CONTAINS in internal proc ignored
            subroutine mffs () bind(c)  ! ign: internal sub in internal sub
            entry mffse () bind(c)  ! no: ENTRY stm disallowed in internal procs
            end subroutine
            function mfff() bind(c)  ! ign: internal function in internal sub
              mfff = 1.0
            entry mfffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
              mfffe = 1.0
            end function

          end function
        end function
      end module

      !! Pogram and all variants it can contain
      program p
      contains
        subroutine ps () bind(c)  ! illegal: internal subroutine in prog
        entry pse () bind(c)  ! no: ENTRY stm disallowed in internal procs
        contains  ! no: CONTAINS stm disallowed in internal procs

          !! code after CONTAINS in internal proc ignored
          subroutine pss () bind(c)  ! ign: internal subroutine in internal sub
          entry psse () bind(c)  ! no: ENTRY stm disallowed in internal procs
          end subroutine
          function psf() bind(c)  ! ign: internal function in internal sub
            psf = 1.0
          entry psfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
            psfe = 1.0
          end function

        end subroutine

        function pf() bind(c)  ! illegal: internal function in prog
          pf = 1.0
        entry pfe() bind(c)  ! no: ENTRY stm disallowed in internal procs
          pfe = 1.0
        contains  ! no: CONTAINS stm disallowed in internal procs

            !! code after CONTAINS in internal proc ignored
            subroutine pfs () bind(c)  ! ign: internal sub in internal sub
            entry pfse () bind(c)  ! no: ENTRY stm disallowed in internal procs
            end subroutine
            function pff() bind(c)  ! ign: internal function in internal sub
              pff = 1.0
            entry pffe() bind(c)  ! no: ENTRY stm disallowed in internal procs
              pffe = 1.0
            end function

        end function
      end
