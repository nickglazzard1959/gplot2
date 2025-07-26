"""
sgformat - A RUNOFF-style document formatter
--------------------------------------------

This program was written to try to format the original DIMFILM manual
by working from the .SG version of the documentation.

I have no idea what .SG files are. It is clearly some RUNOFF style markup format,
but it isn't Digital Standard RUNOFF or anything else I have seen before.

The manual is also available in Wordstar and Microsoft Word format, but the
files are for mid-1980s versions of these programs and I haven't been able to use
them. Maybe .MSW and .WS2 files could be processed by ancient preserved
software running on DOSBox or similar?

The .SG files seemed like the best bet to get something readable. I tried this
in 2008 and got something semi-readable, but pretty poor.

The only way to get anything decent was to write a pretty much complete
document formatter that understood ".SG format" ... although there is a
fair bit of guesswork involved with regard to what .SG format commands
should do.

Using Python to do this in 2025 was much easier that hacking in C in 2008,
and the results are quite decent.

This program can be used to create other documents using the .SG flavour
of RUNOFF-like formatting with output to .txt and .pdf files, in the
unlikely event anyone wants to do that.

The "main commands" are explained in comments. Look for the comment
"BODY COMMAND PROCESSOR" and read the description of each command in
the code that follows.

There are also formatting commands that can be embedded in lines. The ones
we pay attention to are:
  $T - Tab in .display and .copy regions. Otherwise indent by body_tab_size
       if first thing found on a line.
  $I - Further indent by body_indent_size, if not in .display or .copy regions.
       SHould not appear in those regions.
  $C - Centre the following text to the end of the line in "normal regions".
  $E - Right justify the following text.
$C and $E can be used on a single line to split it into 3 sections: left justified,
centered and right justified.

There are two "variables" which can be dynamically substituted:
  ~%date is replaced by a date: today's by default or a specified date,
  ~%page is replaced by the current output page number.

If an input line starts with a single space, insert a paragraph break
before the line.

Some characters must be escaped. Escape is signalled by ~~.
  ~~# --> #   Unescaped #'s are converted to (non-removable) spaces.
  ~~_ --> _   Otherwise _ is removed entirely.
  ~~$ --> $   Otherwise $ is interpreted as starting as "inline command: such as $T.
  ~~~ --> ~   Insert a literal ~. To avoid inadvertantly escaping a following $,
              a space between the ~~~ and $.

Nick Glazzard, July 2025.
"""

import os
import sys
import random
from datetime import date
import subprocess
import argparse
import json
import tempfile

def sgformat(control_file, debug=False):
    """
    Format .SG 'RUNOFF" style document source to palin text and PDF
    output files.
    """
    
    def join_words(words, start=1):
        """
        Concatenate a list of words, putting single spaces between them
        """
        result = ''
        n_words = len(words)
        if start >= n_words:
            return result
        else:
            for i in range(start,n_words):
                if i > start:
                    result += ' '
                result = result + words[i]
            return result

    def output_character_remap(aline):
        """
        Remap certain (mostly Unicode) characters as the last stage of output.
        We use Unicode for tilde escaped characters.
        """
        final_map = {'#':' ', '\u0391':'_', '\u0392':'#', '\u0393':'$', '\u0394':'~'}
        for remap_char in final_map:
            if remap_char in aline:
                aline = aline.replace(remap_char, final_map[remap_char])
        return aline

    def format_page_number( page_number ):
        """
        Format a page number. If positive, just convert to (decimal) string.
        If negative, convert to Roman numerals
        """
        def to_roman_numeral(num):
            val = [
                (1000, 'm'), (900, 'cm'), (500, 'd'), (400, 'cd'),
                (100, 'c'), (90, 'xc'), (50, 'l'), (40, 'xl'),
                (10, 'x'), (9, 'ix'), (5, 'v'), (4, 'iv'), (1, 'i')
            ]
            roman_numeral = ''
            for arabic, roman in val:
                while num >= arabic:
                    roman_numeral += roman
                    num -= arabic
            return '(' + roman_numeral + ')'
        #
        if page_number < 0:
            return to_roman_numeral(abs(page_number))
        else:
            return str(page_number)

    def output_line_format(aline):
        """
        Perform in-line positional formatting in the output line.
        """
        # Things we will deal with.
        flags = ['$c', '$C', '$e', '$E', '~%page', '~%date', '$T']
        process = False
        for flag in flags:
            if flag in aline:
                process = True

        # Return aline unchanged if nothing detected to do.
        if not process:
            return output_character_remap(aline)

        # Modify aline.
        else:

            # Substitute page number and date "variables".
            if '~%page' in aline:
                aline = aline.replace('~%page', format_page_number(page_counter))
            if '~%date' in aline:
                aline = aline.replace('~%date', str(date_string))

            # See if centering and or right aligning parts of aline.
            # Split it into left, center and right substrings.
            # Any of these may be empty.
            aline = aline.replace('$C','$c')
            aline = aline.replace('$E','$e')
            unsplit = False
            if '$c' in aline:
                sL, sC = aline.split('$c')
                if '$e' in sC:
                    sC, sR = sC.split('$e')
                else:
                    sR = ''
            else:
                if '$e' in aline:
                    sL, sR = aline.split('$e')
                    sC = ''
                else:
                    sL = aline
                    sC = ''
                    sR = ''
                    unsplit = True

            # If aline was split, re-assemble substrings with padding
            # to fill the line width.
            if not unsplit:
                lsC = len(sC)
                xC = line_width // 2
                xCs = xC - (lsC // 2)
                xCe = xC + (lsC // 2)
                if (lsC & 1) != 0:
                    xCe += 1
                xLe = len(sL)
                nLCpad = xCs - xLe
                xRs = line_width - len(sR)
                nCRpad = xRs - xCe
                aline = sL + (nLCpad * ' ') + sC + (nCRpad * ' ') + sR

            # Perform tabbing with $T. This will only be seen in .display
            # and .copy regions. $T in body text may mean something else and
            # we have already dealt with that (sort of, maybe).
            if '$T' in aline:
                len_aline = len(aline)
                oline = ''
                len_oline = 0
                tabset_length = len(tabset)
                tab_number = -1
                i = 0
                while i < len_aline:
                    isnt_tab = True
                    if aline[i] == '$':
                        if i < (len_aline-1):
                            if aline[i+1] == 'T':
                                i += 1
                                tab_number += 1
                                tab_number = min(tab_number, tabset_length-1)
                                o_pos = tabset[tab_number]
                                while len_oline < o_pos:
                                    oline += '#'
                                    len_oline += 1
                                isnt_tab = False
                    if isnt_tab:
                        oline += aline[i]
                        len_oline += 1
                    i += 1
                aline = oline[:line_width]
                            
            return output_character_remap(aline)
        
    def add_output_line(aline):
        """
        Append a completed output line (characters) to the output document.
        Track how many lines are on a page. Add header and footer regions
        at the top and bottom of the page. 
        """
        nonlocal page_line_count, page_counter
        
        if page_line_count == 0:
            for head_line in head_def:
                output_lines.append( output_line_format(head_line) )
            output_lines.append(' ')
            
        output_lines.append( output_line_format(aline) )
        page_line_count += 1
        
        if page_line_count == page_lines:
            output_lines.append(' ')
            for foot_line in foot_def:
                output_lines.append( output_line_format(foot_line) )
            output_lines.append('%PAGE%END%')
            page_line_count = 0
            page_counter += 1

    def flush_output_words(suppress_justify=False):
        """
        Given a list of words in output_line_words, join them into
        an output line (characters). Optionally, "justify" the line
        by adding spaces between words until the last character on
        a line is at the line_width. 
        """
        nonlocal output_line_words, len_output_line
        
        output_line_chars = join_words(output_line_words, 0)
        n_output_line_chars = len(output_line_chars)
        
        if n_output_line_chars > 0:
            if justify and (not suppress_justify):
                n_output_line_words = len(output_line_words)
                if n_output_line_words > min_justify_words:
                    n_gaps = n_output_line_words - 1
                    gap_space = [1] * n_gaps
                    while n_output_line_chars < line_width:
                        for k in range(4):
                            i_gap = random.randint(0, n_gaps-1)
                            if gap_space[i_gap] < 3:
                                break
                        gap_space[i_gap] += 1
                        n_output_line_chars += 1
                    output_line_chars = ''
                    for i in range(0, n_output_line_words):
                        if i > 0:
                            output_line_chars += (gap_space[i-1] * ' ')
                        output_line_chars += output_line_words[i]
                        
            add_output_line(output_line_chars)
            
        len_output_line = 0
        output_line_words = []

    def add_input_line(aline):
        """
        Split aline (characters) at spaces to create a list of words.
        Or, if suppress_output, just throw the line away.
        Then add the words to the output_line_words list until the
        accumulated line length exceeds line_width. At that point,
        flush the contents of output_line_words and put the word that
        crossed line_width into an empty output_line_words list.
        """
        nonlocal output_line_words, len_output_line
        
        if suppress_output:
            return
        
        # Split the line into words on spaces.
        input_line_words = aline.split()

        # Were there any TAB or INDENT commands in the line?
        # If so, replace them in each words with some number of #s.
        # TBH, I'm not sure what $T and $I are supposed to do in "body text".
        # In "display text", $T makes sense as tab, but it doesn't in "body text".
        has_tab_indents = ('$T' in aline) or ('$I' in aline)            

        # Iterate over input words. 
        for word in input_line_words:
            if has_tab_indents:
                word = word.replace('$T', body_tab_size*'#')
                word = word.replace('$I', body_indent_size*'#')
                
            lword = len(word)
            if len_output_line > 0:
                lword += 1
            if (len_output_line + lword) > line_width:
                flush_output_words()
                output_line_words.append(word)
                len_output_line += len(word)
            else:
                output_line_words.append(word)
                len_output_line += lword

    def dump_output(outname):
        fout = open(outname, 'w')
        for output_line in output_lines:
            print(output_line, file=fout)
        fout.close()

    def contents_dotfill_append(string, offset=0):
        """
        Construct a Contents line.
        """
        dotsto = line_width - 4 # space  + max no page number digits
        ndots = dotsto - len(string) - 1
        dots = ndots*'.'
        contents.append(string + ' ' + dots + ' ' + str(page_counter+offset))

    def new_page():
        """
        Flush the current page and start a new one.
        """
        nonlocal page_line_count, page_lines
        
        flush_output_words()
        n_lines = page_lines - page_line_count
        for _ in range(n_lines):
            add_output_line(' ')

    def add_hrule():
        """
        Add a horizontal rule to the output directly.
        """
        add_output_line(' ')
        add_output_line( line_width * '-' )
        add_output_line(' ')

    def strip_unsupported_inline_formatting(aline):
        """
        We do not support "appearence" formatting commands (bold, underline).
        Throw them away. Also remap "strange" relational symbols to conventional
        symbols.
        This also maps multi-character escape sequences to single Unicode characters.
        The exact values don't matter (they are Greek letters), as they get mapped
        back before anyone sees them!
        """
        fixups = {'_>_':'>=', '_<_':'<=', '~~_':'\u0391','~~#':'\u0392','~~$':'\u0393','~~~':'\u0394'}
        togo = ['$B', '$b', '_']

        for fixup in fixups.keys():
            if fixup in aline:
                aline = aline.replace(fixup, fixups[fixup])
        for go in togo:
            if go in aline:
                aline = aline.replace(go, '')
        return aline

    def post_process_output():
        """
        Insert the Contents if desired.
        """
        nonlocal page_counter
        
        final_output_lines = []
        output_page_count = 0
        output_page_line_count = 0
        page_counter = -1

        output_lines_count = len(output_lines)
        for i in range(output_lines_count):
            output_line = output_lines[i]
            j = i + 1
            if j < output_lines_count:
                next_output_line = output_lines[j]
            else:
                next_output_line = output_lines[i]

            if insert_contents and (output_page_count == 1):
                for contents_line in contents:
                    if output_page_line_count == 0:
                        for head_line in head_def:
                            final_output_lines.append( output_line_format(head_line) )
                        final_output_lines.append(' ')

                    final_output_lines.append(contents_line)
                    output_page_line_count += 1

                    if output_page_line_count == page_lines:
                        final_output_lines.append(' ')
                        for foot_line in foot_def:
                            final_output_lines.append( output_line_format(foot_line) )
                        final_output_lines.append((line_width // 2) * '. ')
                        output_page_line_count = 0
                        output_page_count += 1
                        page_counter -= 1

                while output_page_line_count < page_lines:
                    final_output_lines.append(' ')
                    output_page_line_count += 1
                    
                final_output_lines.append(' ')
                for foot_line in foot_def:
                    final_output_lines.append( output_line_format(foot_line) )
                    
                final_output_lines.append((line_width // 2) * '. ')
                output_page_line_count = 0
                output_page_count += 1

            else:
                if output_line[:10] == '%PAGE%END%':
                    final_output_lines.append((line_width // 2) * '. ')
                    output_page_line_count = 0
                    output_page_count += 1
                else:
                    final_output_lines.append(output_line)
                    output_page_line_count += 1

        return(final_output_lines)

    def text_final_output(outname, final_output_lines):
        """
        Write the finished document as plain text to outname.
        """
        fout = open(outname, 'w')
        for final_output_line in final_output_lines:
            print(final_output_line, file=fout)
        fout.close()

    def postscript_final_output(outname, final_output_lines):
        """
        Write the finished document as PostScript, potentially for
        further processing to PDF.
        """
        # Define character escapes. The order they are done in matters!
        ps_escapes = {'\\':'\\\\', '(':'\(', ')':'\)'}
        
        # Open output file. Write minimal PostScript header to it.
        fout = open(outname, 'w')
        fout.write("%!PS\n")
        fout.write("/{0} {1} selectfont\n".format(ps_font_name, ps_text_size_points))

        # Initial position at start of page.
        x = ps_left_margin_points
        y = ps_page_top_points
        n_page_lines = 0

        # Write out the final output lines.
        for final_output_line in final_output_lines:
            if final_output_line[:13] == '. . . . . . .':
                fout.write('showpage\n')
                x = ps_left_margin_points
                y = ps_page_top_points
                n_page_lines = 0

            else:
                if y > 0:
                    ps_output_line = final_output_line
                    for ps_escape in ps_escapes.keys():
                        ps_output_line = ps_output_line.replace(ps_escape, ps_escapes[ps_escape])
                    fout.write('{0} {1} moveto ({2}) show\n'.format(x, y, ps_output_line))
                    n_page_lines += 1
                    y -= ps_line_step

        # If anything added to a last page, output the page.
        if n_page_lines > 0:
            fout.write('showpage\n')
        fout.close()

    def pdf_final_output(inname, outname):
        """
        Convert PostScript output to PDF output using ps2pdf from the
        Ghostscript package.
        """
        cmd = ['ps2pdf', '-sPAPERSIZE='+ps_paper, inname, outname]
        try:
            retstring = subprocess.check_output(cmd, universal_newlines=True)
        except Exception as e:
            print('ERROR: Cannot produce PDF output.')
            print('... Reason:', e)

    # Default formatting options.
    page_lines = 58  # Not including header and footer lines!
    line_width = 80
    body_tab_size = 6
    body_indent_size = 3
    
    min_justify_words = 8

    default_tabset = [8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120]

    ps_font_name = 'Courier'
    ps_paper='a4'
    if ps_paper == 'a4':
        ps_page_height_points = 842
        ps_page_width_points = 595
    elif ps_paper == 'letter':
        ps_page_height_points = 791
        ps_page_width_points = 612
    elif ps_paper == 'legal':
        ps_page_height_points = 1009
        ps_page_width_points = 612        
        
    ps_top_margin_points = (3 * 72) // 4
    ps_left_margin_points = (3 * 72) // 4
    ps_text_size_points = 10
    ps_leading = 2

    specific_date_string = ''

    # Read the control file. This is a JSON file that MUST specify the
    # input source files, the source directory, where they can be found,
    # and the output file stem name.
    try:
        jfin = open(control_file)
        control_dict = json.load(jfin)
        jfin.close()
        if debug:
            print('Control file:')
            print(control_dict)
    except Exception as e:
        print('ERROR: Cannot open/parse control JSON file:', control_file)
        print('... Reason:', e)
        sys.exit(1)

    # Use the control file dictionary entries to set or update the
    # source files and formatting options.
    if 'source_files' not in control_dict.keys():
        print('ERROR: source_files must be specified in the control file. Stopping.')
        sys.exit(1)
    else:
        source_files = control_dict['source_files']
    if 'source_dir' not in control_dict.keys():
        print('ERROR: source_dir must be specified in the control file. Stopping.')
        sys.exit(1)
    else:
        source_dir = control_dict['source_dir']
    if 'output_stem' not in control_dict.keys():
        print('ERROR: output_stem must be specified in the control file. Stopping.')
        sys.exit(1)
    else:
        output_stem = control_dict['output_stem']
    if 'output_dir' in control_dict.keys():
        output_dir = control_dict['output_dir']
    else:
        output_dir = '.'
    output_path_stem = os.path.join(output_dir, output_stem)
        
    try:
        if 'page_lines' in control_dict.keys():
            page_lines = int(control_dict['page_lines'])
        if 'line_width' in control_dict.keys():
            line_width = int(control_dict['line_width'])
        if 'body_tab_size' in control_dict.keys():
            body_tab_size = int(control_dict['body_tab_size'])
        if 'body_indent_size' in control_dict.keys():
            body_indent_size = int(control_dict['body_indent_size'])
        if 'min_justify_words' in control_dict.keys():
            min_justify_words = int(control_dict['min_justify_words'])
        if 'ps_page_height_points' in control_dict.keys():
            ps_page_height_points = int(control_dict['ps_page_height_points'])
        if 'ps_page_width_points' in control_dict.keys():
            ps_page_width_points = int(control_dict['ps_page_width_points'])
        if 'ps_top_margin_points' in control_dict.keys():
            ps_top_margin_points = int(control_dict['ps_top_margin_points'])
        if 'ps_left_margin_points' in control_dict.keys():
            ps_left_margin_points = int(control_dict['ps_left_margin_points'])
        if 'ps_text_size_points' in control_dict.keys():
            ps_text_size_points = int(control_dict['ps_text_size_points'])
        if 'ps_leading' in control_dict.keys():
            ps_leading = int(control_dict['ps_leading'])
        if 'ps_font_name' in control_dict.keys():
            ps_font_name = int(control_dict['ps_font_name'])
        if 'specific_date_string' in control_dict.keys():
            specific_date_string = control_dict['specific_date_string']
        if 'ps_paper' in control_dict.keys():
            ps_paper = control_dict['ps_paper']
        if 'default_tabset' in control_dict.keys():
            default_tabset = [int(x) for x in control_dict['default_tabset']]
    except Exception as e:
        print('ERROR: Failed to read values from control file.')
        print('... Reason:', e)
        sys.exit(1)

    # Read all the source into memory. Clean it up along the way.
    # Save the text for each input file as a list of strings, one string per line.
    # Track "body commands" which are lines that start with '.'. Some of these
    # are ignored (for now), but we had to implement enough of them to get the
    # DIMFILM document to process "correctly" (arguably). We therefore needed
    # to know what commands it used.
    phase = "Reading source files."
    print('\n', phase, '\n', len(phase)*'-', '\n')
    doc_dict = {}
    body_commands = []
    for source_file in source_files:
        try:
            full_path = os.path.join(source_dir, source_file)
            with open(full_path) as fin:
                clean_text = []
                for line in fin:
                    clean_line = line[1:].rstrip()
                    clean_text.append(clean_line)
                doc_dict[source_file] = clean_text
                n_body_commands = 0
                for line in doc_dict[source_file]:
                    if len(line) > 0:
                        if line[0] == '.':
                            n_body_commands += 1
                            words = line.split()
                            first_word_lower = words[0].lower()
                            if first_word_lower not in body_commands:
                                body_commands.append(first_word_lower)
                print('...', source_file, 'has', len(doc_dict[source_file]), 'lines, with', n_body_commands, 'body commands.')
        except Exception as e:
            print('Failed to deal with input file:', full_path)
            print('... Reason:', e)
            sys.exit(1)

    #print(body_commands)

    # Do the formatting.
    phase = "Formatting the source text."
    print('\n', phase, '\n', len(phase)*'-', '\n')
    
    # Set the initial state.
    page_counter = 1
    part_counter = 0
    chapter_counter = 0
    section_counter = 0
    subsection_counter = 0
    appendix_counter = 0
    if specific_date_string == '':
        use_date = date.today()
    else:
        try:
            sdi = [int(x) for x in specific_date_string.split(',')]
            use_date = date(*sdi)
        except Exception as e:
            print('ERROR: Cannot parse specific_date_string. Defaulting to today.')
            print(' ... Details:', e)
            use_date = date.today()
    date_string = use_date.strftime("%d/%m/%Y")

    suppress_output = False
    in_head_def = False
    head_def = []
    in_foot_def = False
    foot_def = []
    in_appendices = False
    in_display = False

    justify = False

    page_line_count = 0
    output_lines = []
    output_line_words = []
    len_output_line = 0
    insert_contents = False

    tabset = default_tabset

    ps_page_top_points = ps_page_height_points - ps_top_margin_points
    ps_line_step = ps_text_size_points + ps_leading

    # Prepare to accumulate the contents information.
    contents = []
    contents.append(' ')
    contents.append(((line_width-15)//2)*' ' + 'C O N T E N T S')
    contents.append(' ')

    # Step over input source file names and get their text.
    for source_file in source_files:
        text = doc_dict[source_file]
        n_text_lines = len(text)
        print('['+source_file+']')

        # Step over lines of the current input file text.
        i_line = -1
        for line in text:
            i_line += 1

            # Lose bold and underline "appearance" formatting commands for now.
            line = strip_unsupported_inline_formatting(line)
            #print(line)

            # See if the line has a leading space but is not empty.
            # Signals a paragraph break.
            first_blank = (len(line) > 0) and (line[0] == ' ')

            # Split the input line into space separated words.
            words = line.split()
            n_words = len(words)

            # If the input line is empty, ouput a blank line. Probably.
            if n_words == 0:
                flush_output_words()
                add_output_line(' ')  # or ignore?
                continue

            # BODY COMMAND PROCESSOR
            # See if the first word is a "body command" starting with . and
            # using the whole line.  Process each body command.
            first_word = words[0].lower()

            if first_word == '.':
                # Output a blank line for . (only) as first word
                if in_display:
                    add_output_line(display_pad * '#' + line)
                else:
                    add_output_line(' ')

            elif first_word == '.macro':
                # Ignore .macro definitions (although it would be easy to process them).
                suppress_output = True

            elif first_word == '.endm':
                # End of macro definition.
                suppress_output = False

            elif first_word == '.justify':
                # Turn on justification for smooth (not ragged) right edge.
                justify = True

            elif first_word == '.part':
                # Start of a new Part.
                part_counter += 1
                chapter_counter = 0
                section_counter = 0
                subsection_counter = 0
                contents_line = 'PART {0} - {1}'.format(part_counter, join_words(words))
                contents.append(' ')
                contents.append(contents_line)

                flush_output_words(suppress_justify=True)
                new_page()
                for _ in range(page_lines // 3):
                    add_output_line(' ')
                add_input_line('$c'+contents_line)
                flush_output_words()

            elif first_word == '.chapter':
                # Start of a new Chapter within a Part.
                chapter_counter += 1
                section_counter = 0
                subsection_counter = 0
                contents_line = '   Chapter {0} - {1}'.format(chapter_counter, join_words(words))
                contents_dotfill_append(contents_line, 1)
                
                flush_output_words()
                new_page()
                add_input_line(contents_line.lstrip())
                flush_output_words(suppress_justify=True)
                add_output_line(' ')

            elif first_word == '.section':
                # Start of a new Section within a Chapter.
                section_counter += 1
                subsection_counter = 0
                x_counter = appendix_counter if in_appendices else chapter_counter
                contents_line = '      {0}.{1} - {2}'.format(x_counter, section_counter, join_words(words))
                contents_dotfill_append(contents_line)
                
                flush_output_words()
                add_output_line(' ')
                add_input_line(contents_line.lstrip())
                flush_output_words(suppress_justify=True)
                add_output_line(' ')

            elif first_word == '.subsection':
                # Start of a new Sub-section within a Section.
                subsection_counter += 1
                x_counter = appendix_counter if in_appendices else chapter_counter
                contents_line = '      {0}.{1}.{2} - {3}'.format(x_counter, section_counter, subsection_counter, join_words(words))
                contents_dotfill_append(contents_line)
                
                flush_output_words()
                add_output_line(' ')
                add_input_line(contents_line.lstrip())
                flush_output_words(suppress_justify=True)
                add_output_line(' ')

            elif first_word == '.appendix':
                # Start of a new Appendix. As per Chapter, but keep separate count.
                in_appendices = True
                appendix_counter += 1
                section_counter = 0
                subsection_counter = 0
                contents_line = '   Appendix {0} - {1}'.format(appendix_counter, join_words(words))
                contents_dotfill_append(contents_line, 1)
                
                flush_output_words()
                new_page()
                add_input_line(contents_line.lstrip())
                flush_output_words(suppress_justify=True)
                add_output_line(' ')

            elif first_word == '.foot':
                # Start of Footer text defintion.
                foot_def = []
                in_foot_def = True
                suppress_output = True

            elif first_word == '.endf':
                # End of Footer text definition.
                in_foot_def = False;
                suppress_output = False
                
            elif first_word == '.head':
                # Start of Header text definition.
                head_def = []
                in_head_def = True
                suppress_output = True

            elif first_word == '.endh':
                # End of Header text definition.
                in_head_def = False;
                suppress_output = False

            elif first_word == '.nl':
                # ?
                pass

            elif first_word == '.fill':
                # ?
                pass

            elif first_word == '.contig':
                # ?
                pass

            elif first_word == '.tabset':
                # Define a new set of tab stops.
                if n_words == 1:
                    tabset = default_tabset
                else:
                    tabset_strings = words[1].split(',')
                    tabset = [int(s) for s in tabset_strings]
            
            elif (first_word == '.endd') or (first_word == '.endc'):
                # End of .display or .copy literal section.
                add_hrule()
                in_display = False
            
            elif (first_word == '.display') or (first_word == '.copy'):
                # Literal section for ASCII art or code that should not
                # be split across pages. .display centers horizontally.
                # .copy does not. If tabs commands are found in a .display,
                # treat it as a .copy. I'm not sure if this is the correct
                # behaviour, but it looks pretty good.
                in_display = True
                flush_output_words()

                n_display_lines = 0
                max_display_width = 0
                tabs_in_display = False
                for i_temp in range(i_line+1, n_text_lines):
                    temp_line = text[i_temp]
                    tabs_in_display = tabs_in_display or ( ('$T' in temp_line) or ('$t' in temp_line) )
                    temp_words = temp_line.split()
                    if (len(temp_words) > 0) and ((temp_words[0].lower() == '.endd') or (temp_words[0].lower() == '.endc')):
                        break
                    else:
                        n_display_lines += 1
                        max_display_width = max(max_display_width, len(text[i_temp]))

                if (first_word == '.display') and (not tabs_in_display):
                    display_pad = max(0, (line_width - max_display_width) // 2)
                else:
                    display_pad = 0

                page_remains = page_lines - page_line_count
                # print('... Display with', n_display_lines, 'lines. Remaining on page:', page_remains, 'Width',max_display_width)
                if (page_lines - n_display_lines) < 1:
                    print('... ... WARNING: Display is too big for the page size! Input line:', i_line+1)
                    print('... ... ... Page lines:', page_lines, 'Display lines:', n_display_lines)

                if (page_remains - n_display_lines) < 1:
                    new_page()

                add_hrule()

            elif first_word == '.ee':
                # EE Macro invocation. Ignore.
                # This is defined and heavily used in the DIMFILM document, but not needed.
                pass

            elif first_word == '.tt':
                # TT Macro invocation. Ignore.
                # This is defined and heavily used in the DIMFILM document, but not needed.
                pass

            elif first_word == '.page':
                # Set page number.
                if n_words == 1:
                    page_counter = 0
                else:
                    page_counter = int(words[1])

            elif first_word == '.space':
                # General line spacing? Ignore.
                pass
            
            elif first_word == '.newpage':
                # Go to a new page.
                new_page()

            elif first_word == '.blank':
                # Insert a specified number of blank output lines.
                flush_output_words()
                if n_words == 1:
                    n_blanks = 1
                else:
                    n_blanks = int(words[1])
                for _ in range(n_blanks):
                    add_output_line(' ')

            elif first_word == '.footlength':
                # Set length of Footer. Not needed.
                pass

            elif first_word == '.headlength':
                # Set length of Header. Not needed.
                pass

            elif first_word == '.set':
                # Set various things. E.g. current chapter number,  etc.
                # Currently ignore.
                pass

            elif first_word == '.parspace':
                # Number of blank lines after paragraphs?
                # Currently ignore.
                pass

            elif first_word == '.table':
                # ?
                pass

            elif first_word == '.endtable':
                # ?
                pass

            elif first_word == '.contents':
                # Insert Contents into the finished document.
                insert_contents = True

            elif (first_word[0] == '.') and (not in_display):
                # Unhandled "body command".
                print('WARNING: Unknown body command:', first_word, 'Ignored. Input line:', i_line+1)

            else:
                # Input line is not a "body command" line.
                if in_head_def:
                    # Add line to Header definition.
                    head_def.append(line)
                    
                elif in_foot_def:
                    # Add line to Footer defintion.
                    foot_def.append(line)
                    
                else:
                    # Add the input line to the output, processing any "inline commands".
                    if in_display:
                        # For .display and .copy regions, make a single output line from a
                        # single input line.
                        add_output_line(display_pad * '#' + line)
                        
                    else:
                        # "Ordinary" input lines.
                        if first_blank:
                            # Paragraph break.
                            flush_output_words()
                            add_output_line(' ')
                            
                        add_input_line(line)

    # Post-process the output.
    final_output_lines = post_process_output()

    phase = "Generating output files."
    print('\n', phase, '\n', len(phase)*'-', '\n')

    if debug:
        dump_output('sgdump.txt')
    text_final_output(output_path_stem+'.txt', final_output_lines)
    print('INFO: Wrote plain text to:', output_path_stem+'.txt')

    with tempfile.NamedTemporaryFile() as ftemp:
        postscript_final_output(ftemp.name, final_output_lines)
        pdf_final_output(ftemp.name, output_path_stem+'.pdf')
    print('INFO: Wrote PDF document to:', output_path_stem+'.pdf')

    print('Done.')


def main():
    """
    Mainline.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("ctrlname", help="JSON format control file describing the document to format.")
    parser.add_argument("-d","--debug", help="Turn on debug output.", action="store_true")
    args = parser.parse_args()

    sgformat(args.ctrlname, debug=args.debug)
    sys.exit(0)
    
if __name__ == '__main__':

    main()
    
