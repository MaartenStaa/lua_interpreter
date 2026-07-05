use std::collections::HashMap;

use crate::chunk::Chunk;
use super::protocol::SourceMap;

pub(crate) fn build_source_map(chunk: &Chunk) -> SourceMap {
    let line_starts = compute_line_starts(&chunk.source);

    let mut line_to_ips: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut ip_to_line: HashMap<usize, usize> = HashMap::new();

    for (&ip, span) in &chunk.instruction_spans {
        let line = offset_to_line(&line_starts, span.start);
        line_to_ips.entry(line).or_default().push(ip);
        ip_to_line.insert(ip, line);
    }

    for ips in line_to_ips.values_mut() {
        ips.sort_unstable();
    }

    SourceMap {
        line_to_ips,
        ip_to_line,
    }
}

fn compute_line_starts(source: &[u8]) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, &byte) in source.iter().enumerate() {
        if byte == b'\n' {
            starts.push(i + 1);
        }
    }
    starts
}

fn offset_to_line(line_starts: &[usize], offset: usize) -> usize {
    match line_starts.binary_search(&offset) {
        Ok(line) => line + 1,
        Err(line) => line,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_line() {
        let source = b"local x = 10\nlocal y = 20\nprint(x + y)\n";
        let starts = compute_line_starts(source);
        assert_eq!(starts, vec![0, 14, 28, 41]);

        assert_eq!(offset_to_line(&starts, 0), 1);
        assert_eq!(offset_to_line(&starts, 5), 1);
        assert_eq!(offset_to_line(&starts, 13), 1);
        assert_eq!(offset_to_line(&starts, 14), 2);
        assert_eq!(offset_to_line(&starts, 28), 3);
    }
}
