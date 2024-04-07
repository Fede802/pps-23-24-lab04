package tasks.mvc;

import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

class SwingFunctionalFacade {

    public static interface Frame {
        Frame setSize(int width, int height);
        Frame addButton(String text, String name);
        Frame addLabel(String text, String name);
        Frame addTextField(String text, String name);
        Frame showToLabel(String text, String name);
        String textFromField(String name);
        Frame resetWindow();
        Frame disable(String name);
        Frame show();
        Supplier<String> events();        
    }

    public static Frame createFrame(){
        return new FrameImpl();
    }

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private final Map<String, JComponent> components = new HashMap<>();
        private final Map<String, JButton> buttons = new HashMap<>();
        private final Map<String, JLabel> labels = new HashMap<>();
        private final Map<String, JTextField> fields = new HashMap<>();
        private final LinkedBlockingQueue<String> eventQueue = new LinkedBlockingQueue<>();
        private final Supplier<String> events = () -> {
            try{
                return eventQueue.take();
            } catch (InterruptedException e){
                return "";
            }
        };
        public FrameImpl() {
            this.jframe.setLayout(new FlowLayout());
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addButton(String text, String name) throws IllegalArgumentException {
            JButton jb = new JButton(text);
            jb.setActionCommand(name);
            if(components.containsKey(name)){throw new IllegalArgumentException("Component with name " + name + " already exists");}
            this.components.put(name, jb);
            this.buttons.put(name, jb);
            jb.addActionListener(e -> {
                try {
                    eventQueue.put(name);
                } catch (InterruptedException ex){}
            });
            this.jframe.getContentPane().add(jb);
            return this;
        }

        @Override
        public Frame addLabel(String text, String name) {
            JLabel jl = new JLabel(text);
            if(components.containsKey(name)){throw new IllegalArgumentException("Component with name " + name + " already exists");}
            this.components.put(name, jl);
            this.labels.put(name, jl);
            this.jframe.getContentPane().add(jl);
            return this;
        }

        @Override
        public Frame addTextField(String text, String name) {
            JTextField jtf = new JTextField(text);
            jtf.setPreferredSize(new Dimension(100, 20));
            if(components.containsKey(name)){throw new IllegalArgumentException("Component with name " + name + " already exists");}
            this.components.put(name, jtf);
            this.fields.put(name, jtf);
            this.jframe.getContentPane().add(jtf);
            return this;
        }

        @Override
        public Supplier<String> events() {
            return events;
        }

        @Override
        public Frame showToLabel(String text, String name) {
            this.labels.get(name).setText(text);
            return this;
        }

        @Override
        public String textFromField(String name) {
            return this.fields.get(name).getText();
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

        @Override
        public Frame resetWindow() {
            this.components.forEach((k,v) -> v.setEnabled(true));
            this.fields.forEach((k,v) -> v.setText(""));
            return this;
        }

        @Override
        public Frame disable(String name) {
            this.components.get(name).setEnabled(false);
            return this;
        }

    }
}
