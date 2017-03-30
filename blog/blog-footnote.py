#!/usr/bin/env python

import argparse
import base64
import hashlib
import logging
import os.path
import re

if __name__ == '__main__':
    argparser = argparse.ArgumentParser(
        description='Edits markdown to support footnotes.')
    argparser.add_argument('input_path')
    argparser.add_argument('output_path')
    argparser.add_argument('-v', '--verbose', action='store_true')
    argparser.add_argument('-vv', '--debug', action='store_true')

    args = argparser.parse_args()

    if not os.path.exists(args.input_path):
        raise Exception('Given file does not exist.')

    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
    elif args.verbose:
        logging.basicConfig(level=logging.INFO)

    input_file = open(args.input_path)
    contents = input_file.read()
    paragraphs = contents.split('\n\n')  # Split by paragraph.
    assert len(paragraphs) > 0, 'Nothing in file to read.'
    logging.info('Read %d paragraphs', len(paragraphs))
    logging.debug('%s', str(paragraphs))

    # Prepare outputs.
    output_paragraphs = []
    output_footnotes = []

    # Where are the footnote paragraphs?
    footnote_starts = [
        re.match(r'\[\d+\]', p).group(0) if re.match(r'\[\d+\]', p) else False
        for p in paragraphs]
    assert not footnote_starts[0], 'Starting the document with a footnote.'
    logging.debug('%s', footnote_starts)
    logging.info('Found %d footnotes', len(filter(lambda x: x, footnote_starts)))
    # Where are the continuing paragraphs after footnotes?
    footnote_continues = []
    in_footnote = False
    for p,fs in zip(paragraphs, footnote_starts):
        indented = p.startswith('  ')  # 4 spaces is a quotation, avoid by
                                       # looking for 2 spaces.
        in_footnote = fs or (in_footnote and indented)
        footnote_continues.append(in_footnote)

    file_hash = hashlib.sha256()
    file_hash.update(os.path.basename(args.input_path))
    global_file_id = base64.urlsafe_b64encode(file_hash.digest())[:8]
    logging.info('Using file ID of %s' % global_file_id)
    global_footnote_count = 1  # Global footnote counter.
    for i,paragraph in enumerate(paragraphs):
        # Skip footnotes, should have already processed them.
        if footnote_starts[i] or footnote_continues[i]:
            logging.debug('Paragraph %d is a footnote paragraph, skipping.', i)
            continue

        # How many footnotes do we have following this paragraph?
        footnote_region_start = i + 1
        footnote_region_end = footnote_region_start
        while (footnote_region_end < len(paragraphs) and  # Overrun check.
               (footnote_starts[footnote_region_end] or
                footnote_continues[footnote_region_end])):
            footnote_region_end += 1
            if footnote_region_end > len(paragraphs):
                break
        # No footnotes.
        if (footnote_region_end == footnote_region_start
            or footnote_region_end > len(paragraphs)):
            logging.debug('Paragraph %d has no attached footnotes.', i)
            output_paragraphs.append(paragraph)
            continue
        logging.debug('Paragraph %d has footnotes until paragraph %d',
                      i, footnote_region_end)

        # Find the textual order of the footnotes.
        footnotes_info = []
        # First, find all the footnotes, where they exist, and what
        # paragraphs they map to.
        footnote_obj = {}
        for j in range(footnote_region_start, footnote_region_end):
            if footnote_starts[j]:
                if footnote_obj != {}:
                    footnote_obj['footnote_end'] = j
                    footnotes_info.append(footnote_obj)
                    footnote_obj = {}
                footnote_paragraph_index = paragraph.find(footnote_starts[j])
                footnote_obj = {
                    'footnote_start': j,
                    'footnote_index': footnote_paragraph_index,
                }
                # Sanity check that footnotes exist in the text.
                if footnote_paragraph_index == -1:
                    logging.warning('Paragraph %d, footnote %s does not exist.',
                                    i, footnote_starts[j])
                # Sanity check for duplicated footnotes.
                if paragraph.find(footnote_starts[j],
                                  footnote_paragraph_index + 1) != -1:
                    logging.warning('Paragraph %d, footnote %s is duplicated.',
                                    i, footnote_starts[j])
        # Clean up the last footnote.
        if footnote_obj != {}:
            footnote_obj['footnote_end'] = footnote_region_end
            footnotes_info.append(footnote_obj)
        logging.info('Paragraph %d has %d footnotes attached',
                     i, len(footnotes_info))
        # Sort the footnotes.
        footnotes_info = sorted(footnotes_info,
                                key = lambda x:x['footnote_index'])

        # Munge the footnotes and paragraph.
        modified_paragraph = paragraph
        for footnote_info in footnotes_info:
            f_start = footnote_info['footnote_start']
            f_end = footnote_info['footnote_end']
            footnote_marker = footnote_starts[f_start]
            logging.debug('Paragrah %d: handling footnote %s as number %d',
                          i, footnote_marker, global_footnote_count)

            # Create unique footnote id/return.
            footnote_id = 'footnote-{}-{}'.format(
                global_file_id, global_footnote_count)
            return_id = '{}-return'.format(footnote_id)

            # Munge the paragraph.
            link_to_footnote = '<a href="#{0}" id="{1}">{2}</a>'.format(
                footnote_id, return_id, global_footnote_count)
            modified_paragraph = modified_paragraph.replace(
                footnote_marker, '[{}]'.format(link_to_footnote))

            # Munge the footnote.
            modified_footnote_marker = '[{}]'.format(global_footnote_count)
            link_to_return = '<a href="#{0}" id="{1}">&#8593;</a>'.format(
                return_id, footnote_id)
            footnote_paragraphs = paragraphs[f_start:f_end]
            footnote_paragraphs[0] = footnote_paragraphs[0].replace(
                footnote_marker,
                modified_footnote_marker + ' ' + link_to_return)

            output_footnotes.extend(footnote_paragraphs)

            global_footnote_count += 1

        output_paragraphs.append(modified_paragraph)
        logging.debug('Paragraph %d: done with footnote edits', i)

    # Output the munged file.
    logging.debug('Writing output file...')
    output = ('\n\n'.join(output_paragraphs) +
              '\n\n<hr/>\n\n' +
              '\n\n'.join(output_footnotes) +
              '\n')
    output_file = open(args.output_path, 'w')
    output_file.write(output)
    logging.debug('File written.')
