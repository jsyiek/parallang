import os
import shutil
import subprocess
import sys

try:
    import git
    import jdk
except ImportError:
    print("[PREP] Missing some modules - going to install them, wait a second...")
    subprocess.run([sys.executable, "-m", "pip", "install", "gitpython", "--target", "."])
    subprocess.run([sys.executable, "-m", "pip", "install", "install-jdk", "--target", "."])
    print("[RETRY] Run this again.")
    exit()

JAVA_DIR = "java"
GIT_REPO = "https://github.com/jsyiek/parallang"
REPO_DIR = "parallang"
DATA_DIR = "data"
SPEC_FILE_DIR = os.path.join(REPO_DIR, "notebooks", "spec_files")

print("===================================================")
print("Thank you so much for helping with my dissertation!")
print("         B. M. Syiek (2024) bms53@cam.ac.uk        ")
print("      https://github.com/jsyiek/parallang.git      ")
print("===================================================")

if not os.path.exists(JAVA_DIR):
    print("[PREP] Trying to install Java 21")
    if os.path.exists(JAVA_DIR):
        shutil.rmtree(JAVA_DIR)
    jdk.install("21", jre=True, path=JAVA_DIR)
    # os.rename(os.path.join(local_java_install, "bin", "java"), "dissertation_java")
else:
    print("[PREP] Ah, I think you already have Java 21 installed. Nice.")

local_java_install = os.path.abspath(os.path.join(JAVA_DIR, [s for s in os.listdir(JAVA_DIR) if s.startswith("jdk")][0]))
JAVA_BIN_FILE = os.path.join(local_java_install, "Contents", "Home", "bin", "java")
# os.environ["JAVA_HOME"] = local_java_install



class Run:
    def __init__(self, program, q, dataset, data_out_dir, params, samples):
        self.program = program
        self.q = q
        self.dataset = dataset
        self.data_out_dir = data_out_dir
        self.params = params
        self.samples = samples

if __name__ == "__main__":

    if os.path.exists(DATA_DIR):
        os.makedirs("previous_runs", exist_ok=True)
        i = 0
        while os.path.exists(os.path.join("previous_runs", DATA_DIR + str(i))):
            i += 1
        os.rename(DATA_DIR, os.path.join("previous_runs", DATA_DIR + str(i)))
        print("[PREP] Oh, you have some leftover data from a previous run.", f"I've moved this to {DATA_DIR + str(i)}")

    os.makedirs(DATA_DIR)

    if os.path.exists(REPO_DIR):
        print("[PREP] Deleting old dissertation clone")
        shutil.rmtree(REPO_DIR)

    print("[PREP] Cloning my dissertation...")
    repo = git.Repo.clone_from(GIT_REPO, REPO_DIR)

    spec_file = input("Specification file name >>> ")

    spec_file = os.path.join(SPEC_FILE_DIR, spec_file)

    if not os.path.exists(spec_file):
        print("[ERROR] I don't recognize this spec file. Can you try again?")
        exit()

    runs = []
    # data format:
    # program q dataset data_out_dir params samples
    with open(spec_file, "r") as F:
        for l in F.readlines():
            l = l.rstrip().split("  ")
            runs.append(Run(*l))

    print("[LAUNCH] Starting runs...")
    diss_path = os.path.abspath(os.path.join(REPO_DIR, "out", "artifacts", "APSPEntry_jar", "Dissertation.jar"))
    prog_dir = os.path.abspath(os.path.join(REPO_DIR, "src", "main", "parallang"))
    data_dir = os.path.abspath(os.path.join(REPO_DIR, "notebooks", "datasets"))

    for i, r in enumerate(runs):
        print(f"[RUN] {(i+1)}/{len(runs)}: {r.program} @ {r.dataset} with {r.q}x{r.q} "
              f"threads using {r.params} parameter set ({r.samples} samples)")
        prog = os.path.join(prog_dir, r.program)
        dataset = os.path.join(data_dir, r.dataset)
        data_out_dir = os.path.join(DATA_DIR, r.data_out_dir)
        os.makedirs(data_out_dir, exist_ok=True)
        subprocess.run([JAVA_BIN_FILE, "-jar", diss_path, r.q, prog, dataset, data_out_dir, r.params, r.samples], cwd=os.getcwd())
