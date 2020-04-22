package de.uniba.sme.bambirds.debugging.commands;

import org.springframework.shell.standard.ShellMethod;
import org.springframework.shell.standard.ShellComponent;

@ShellComponent
public class LevelSelectionCommands {

    @ShellMethod("Add two integers together.")
    public int add(int a, int b) {
        return a + b;
    }
}