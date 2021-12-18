#[derive(Default, Debug)]
pub struct JobQueue2<A, S> {
    jobs: Vec<A>,
    state: S,
}
impl<A: std::fmt::Debug, S> JobQueue2<A, S> {
    pub fn new(state: S, iter: impl IntoIterator<Item = A>) -> JobQueue2<A, S> {
        JobQueue2 {
            state,
            jobs: iter.into_iter().collect(),
        }
    }

    pub fn prepend_run_rev<F>(mut self, run: F) -> S
    where
        F: Fn(A, &mut S) -> Box<dyn Iterator<Item = A>>,
    {
        while !self.is_done() {
            let job = self.jobs.remove(0);
            self.jobs.splice(0..0, run(job, &mut self.state));
            //println!("jobs {:?}", self.jobs);
        }
        self.state
    }

    fn is_done(&self) -> bool {
        self.jobs.is_empty()
    }
}
