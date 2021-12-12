#[derive(Default, Debug)]
pub struct JobQueue<A, S> {
    jobs: Vec<A>,
    state: S,
}
impl<A, S> JobQueue<A, S> {
    pub fn new(state: S, iter: impl IntoIterator<Item = A>) -> JobQueue<A, S> {
        JobQueue {
            state,
            jobs: iter.into_iter().collect(),
        }
    }

    pub fn run<F>(mut self, run: F) -> S
    where
        F: Fn(A, &mut S) -> Vec<A>,
    {
        while !self.is_done() {
            let jobs = std::mem::take(&mut self.jobs);
            for job in jobs {
                self.jobs.extend(run(job, &mut self.state));
            }
        }
        self.state
    }

    fn is_done(&self) -> bool {
        self.jobs.is_empty()
    }
}
