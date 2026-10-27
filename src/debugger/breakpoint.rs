use std::collections::HashSet;

use super::protocol::Breakpoint;
use super::source_map;
use crate::{chunk::Chunk, vm::ChunkIndex};

pub(crate) struct BreakpointSet {
    resolved: HashSet<(ChunkIndex, usize)>,
    specs: Vec<Breakpoint>,
}

impl BreakpointSet {
    pub(crate) fn new() -> Self {
        Self {
            resolved: HashSet::new(),
            specs: Vec::new(),
        }
    }

    pub(crate) fn is_hit(&self, chunk: ChunkIndex, ip: usize) -> bool {
        self.resolved.contains(&(chunk, ip))
    }

    pub(crate) fn add(&mut self, bp: Breakpoint, chunks: &[Chunk]) {
        match &bp {
            Breakpoint::Instruction { chunk, ip } => {
                self.resolved.insert((*chunk, *ip));
            }
            Breakpoint::SourceLine { filename, line } => {
                self.resolve_source_line(filename, *line, chunks);
            }
        }
        self.specs.push(bp);
    }

    pub(crate) fn remove(&mut self, bp: &Breakpoint) {
        self.specs.retain(|s| s != bp);
        self.rebuild_resolved_from_specs();
    }

    pub(crate) fn specs(&self) -> &[Breakpoint] {
        &self.specs
    }

    fn resolve_source_line(&mut self, filename: &str, line: usize, chunks: &[Chunk]) {
        for (chunk_idx, chunk) in chunks.iter().enumerate() {
            let chunk_filename = chunk
                .filename
                .as_ref()
                .and_then(|p| p.file_name())
                .and_then(|n| n.to_str());

            let matches = chunk_filename.is_some_and(|n| n == filename)
                || chunk
                    .filename
                    .as_ref()
                    .and_then(|p| p.to_str())
                    .is_some_and(|p| p == filename);

            if !matches {
                continue;
            }

            let source_map = source_map::build_source_map(
                &chunk.instructions,
                &chunk.instruction_spans,
                &chunk.source,
            );
            if let Some(ips) = source_map.line_to_ips.get(&line)
                && let Some(&first_ip) = ips.first()
            {
                self.resolved.insert((chunk_idx as ChunkIndex, first_ip));
            }
        }
    }

    fn rebuild_resolved_from_specs(&mut self) {
        self.resolved.clear();
        for spec in &self.specs {
            if let Breakpoint::Instruction { chunk, ip } = spec {
                self.resolved.insert((*chunk, *ip));
            }
        }
    }
}
