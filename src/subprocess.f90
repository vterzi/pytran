module stdlib_subprocess
    use stdlib_string, only: operator(//)
    use stdlib_io, only: tmp_filename, read_file

    implicit none

contains
    subroutine run(command, stdout, stderr, success)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out), optional :: stdout, stderr
        logical, intent(out), optional :: success

        integer :: exitstat, cmdstat
        character(len=0) :: cmdmsg
        character(len=:), allocatable :: cmd, outfile, errfile

        cmd = command
        if (present(stdout)) then
            outfile = tmp_filename()
            cmd = cmd // ' 1>' // outfile
        else
            outfile = ''
        end if
        if (present(stderr)) then
            errfile = outfile
            do while(errfile == outfile)
                errfile = tmp_filename()
            end do
            cmd = cmd // ' 2>' // errfile
        else
            errfile = ''
        end if
        call execute_command_line( &
            cmd, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg &
        )
        if (len(outfile) > 0) stdout = read_file(outfile, .true.)
        if (len(errfile) > 0) stderr = read_file(errfile, .true.)
        if (present(success)) success = exitstat == 0
    end subroutine run
end module stdlib_subprocess
