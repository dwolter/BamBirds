package de.uniba.sme.bambirds.debugging;

import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.shell.jline.PromptProvider;

import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.PrologPlanner;

@SpringBootApplication
public class BamBirdsShell {
	public static void main(String[] args) {
		Settings.load(args);
		PrologPlanner.compileExecutable();
		SpringApplication.run(BamBirdsShell.class, args);
	}

	@Bean
	public PromptProvider myPromptProvider() {
		return () -> new AttributedString("bambirds:>", AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW));
	}
}