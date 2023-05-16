import os

root = "F:/"
# root = '/home/yslin/'
wkdir = root + "[01]Projects/viscando/tests/"
# Set working directory to debug using interactive environment.
os.chdir(wkdir)

import sc_run
import sc_fitting
import numpy as np

# use multi cores to run each instance of a for loop in python
from joblib import Parallel, delayed
import multiprocessing
from multiprocessing import Pool


# SuperFastPython.com. example of a sequential for loop
from time import sleep
from random import random

# Correct the missing font problem.
from matplotlib import rcParams

rcParams.update(
    {
        "font.family": "sans-serif",
        "font.sans-serif": ["Liberation Sans"],
    }
)

# 1. It is unclear how the "initial" in the naturalitic setting matching to
# the model. I selected the first instance of non-zero, meaning the
# Viscando sensor returned the first valid sample.
# 2. The "end_time" is set to 10 seconds in GM's model.

# Read a csv file. I used the manual selection, because there were only four.
# This could be a if-else. To save time and make it clear, I did not
# use a if-else or case syntax.
# fn0 = wkdir + 'extdata/4_short_stop.csv' <- shapr deceleration
# fn1 = wkdir + 'extdata/5_priority_assertion.csv'
# fn2 = wkdir + 'extdata/6_hesitation.csv'
# fn3 = wkdir + 'extdata/7_early_yield.csv'
fn = wkdir + "extdata/4_short_stop_correct.csv"
data = np.genfromtxt(fn, delimiter=",", names=True)

# The name of the four columns: distance_ped, distance_veh, speed_ped, speed_veh.
# The number of rows:
ninteraction = data.shape[0]

N_AGENTS = 2
# pyroot =  'pydata/short_stop'
# pyroot =  'pydata/priority_assertion'
# pyroot =  'pydata/hesitation'
# pyroot =  'pydata/early_yield'
pyroot = "pydata/short_stop_correct"


# task to execute in another process
def task(arg1):
    distance_ped = data[arg1][0]
    distance_veh = data[arg1][1]
    speed_ped = data[arg1][2]
    speed_veh = data[arg1][3]

    # Use his "oVAoBEvoAI oDA oSNvoPF" model
    stoch_model = sc_run.get_model_with_params("oVAoBEvoAIoDAoSNvoPF")

    for j in range(0, stoch_model.n_parameterisations):
        sim = sc_run.run_simulation(
            stoch_model,
            initial_cp_distances=(distance_ped, distance_veh),
            initial_speeds=(speed_ped, speed_veh),
            end_time=10,
            ped_prio=True,
            idx_parameterisation=j,
        )
        fn = pyroot + "/time_stamps" + str(arg1) + "-" + str(j) + ".txt"
        np.savetxt(fn, sim.time_stamps, delimiter=",")

        fn = pyroot + "/actual_end_time_" + str(arg1) + "-" + str(j) + ".txt"
        g = list()
        g.append(sim.actual_end_time)
        np.savetxt(fn, np.array(g), fmt="%.8f")

        i_plot_agents = range(N_AGENTS)
        agent_alpha = np.ones(N_AGENTS)
        for idx_agent, i_agent in enumerate(i_plot_agents):
            agent = sim.agents[i_agent]
            alpha = agent_alpha[idx_agent]
            acceleration = agent.trajectory.long_acc
            speed = agent.trajectory.long_speed
            distance2encounter = agent.signed_CP_dists
            other_perceived_speed = agent.other_agent.perception.states.x_perceived[
                1, :
            ]
            other_perceived_distance2encounter = (
                agent.other_agent.perception.states.x_perceived[0, :]
            )
            fn = (
                pyroot
                + "/acceleration_case"
                + str(arg1)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, acceleration, delimiter=",")
            fn = (
                pyroot
                + "/speed"
                + str(arg1)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, speed, delimiter=",")
            fn = (
                pyroot
                + "/other_perceived_speed"
                + str(arg1)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, other_perceived_speed, delimiter=",")
            fn = (
                pyroot
                + "/distance2encounter"
                + str(arg1)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, distance2encounter, delimiter=",")
            fn = (
                pyroot
                + "/other_distance2encounter"
                + str(arg1)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, other_perceived_distance2encounter, delimiter=",")
    # return the generated value
    print(f"{arg1} has a vehicle speed {speed_veh}", flush=True)
    return speed_veh


if __name__ == "__main__":
    # create the process pool
    with Pool() as pool:
        # call the same function with different data in parallel
        for result in pool.imap(task, range(ninteraction)):
            # report the value to show progress
            print(result)
