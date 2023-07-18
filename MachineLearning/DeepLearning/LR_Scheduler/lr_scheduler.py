import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
import matplotlib.pyplot as plt 
import pytorch_lightning as pl
from torchinfo import summary
import torch.optim.lr_scheduler as lr_scheduler
import torch.optim as optim
import torchmetrics
import numpy as np
import copy
from torchinfo import summary
from torch.utils.data import Dataset

if torch.cuda.is_available():
    print("Things will be fast")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

train_transforms =  transforms.Compose([transforms.Resize(256),
     # if size is int, smaller edge of image will be matched to this int
     transforms.RandomCrop((224, 224)),
     transforms.ToTensor()])

val_transforms =  transforms.Compose([transforms.Resize(256),
     # if size is int, smaller edge of image will be matched to this int
     transforms.CenterCrop((224, 224)),
     transforms.ToTensor()])

test_transforms =  transforms.Compose([transforms.Resize(224),
     # if size is int, smaller edge of image will be matched to this int
     transforms.ToTensor()])

train = torchvision.datasets.MNIST(root = './Data', download=True, train = True)
test_set =  torchvision.datasets.MNIST(root = './Data', download=True, train = False,
                                       transform=test_transforms)

train_subset, val_subset = torch.utils.data.random_split(
        train, [50000, 10000], generator=torch.Generator().manual_seed(1))

class MyDataset(Dataset):
    def __init__(self, subset, transform=None):
        self.subset = subset
        self.transform = transform
        
    def __getitem__(self, index):
        x, y = self.subset[index]
        if self.transform:
            x = self.transform(x)
        return x, y
        
    def __len__(self):
        return len(self.subset)

train_set = MyDataset(train_subset, train_transforms)
val_set = MyDataset(val_subset, val_transforms)
train_loader = torch.utils.data.DataLoader(dataset = train_set, batch_size = 64, shuffle = True,
                                           num_workers = 32)
val_loader = torch.utils.data.DataLoader(dataset = val_set, batch_size = 64, shuffle = False,
                                           num_workers = 32)
test_loader = torch.utils.data.DataLoader(dataset = test_set, batch_size = 128, shuffle = False,
                                          num_workers = 32)



# trainiter = iter(train_loader)
# images, labels = next(trainiter)
# print(images.size())
# fig = plt.figure(figsize = (15, 5))
# for idx in np.arange(20):
#     ax = fig.add_subplot(4, int(20/4), idx + 1, xticks = [], yticks = [])
#     ax.imshow(np.squeeze(images[idx]))
#     ax.set_title(labels[idx].item())
#     fig.tight_layout()
# plt.savefig("MNIST.png")
# plt.show()


class AlexNet(nn.Module):
    def __init__(self, num_classes = 10):
        super(AlexNet, self).__init__()
        self.conv = nn.Sequential(
            nn.LazyConv2d(96, kernel_size=11, stride=4, padding=1),
            nn.ReLU(), nn.MaxPool2d(kernel_size=3, stride=2),
            nn.LazyConv2d(256, kernel_size=5, padding=2), nn.ReLU(),
            nn.MaxPool2d(kernel_size=3, stride=2),
            nn.LazyConv2d(384, kernel_size=3, padding=1), nn.ReLU(),
            nn.LazyConv2d(384, kernel_size=3, padding=1), nn.ReLU(),
            nn.LazyConv2d(256, kernel_size=3, padding=1), nn.ReLU(),
            nn.MaxPool2d(kernel_size=3, stride=2))
        self.fc = nn.Sequential(nn.Flatten(),
            nn.LazyLinear(4096), nn.ReLU(), nn.Dropout(p=0.5),
            nn.LazyLinear(4096), nn.ReLU(),nn.Dropout(p=0.5),
            nn.LazyLinear(num_classes)) 
    def forward(self, x):
        return self.fc(self.conv(x))    
    
    
print(summary(AlexNet(), (1,1,224,224)))



def train(model, lr = 0.01, momentum = 0.9, num_epochs = 10):
    model = model.to(device)
    optimizer = optim.SGD(model.parameters(), lr = lr,  momentum = momentum)
    loss = torch.nn.CrossEntropyLoss()
    lambda1 = lambda epoch: 0.95
    scheduler = lr_scheduler.MultiplicativeLR(optimizer, lambda1)
    train_loss = []; train_acc = []; val_loss = []; val_acc = []; test_loss = []; test_acc = []
    best_val_acc = 0.0
    
    for epoch in range(num_epochs):
        running_train_loss = 0.0
        running_val_loss = 0.0
        running_test_loss = 0.0
        correct_train = 0
        
        for x, t in train_loader:
            optimizer.zero_grad()
            z = model(x.to(device))
            J_train = loss(z, t.to(device))
            J_train.backward()
            optimizer.step()
            _, pred = torch.max(z, 1)
            correct_train += (pred == t.to(device)).sum().item()
            running_train_loss += J_train.item()*x.size(0)
        train_acc_ep = correct_train/len(train_set)
        train_loss_ep = running_train_loss/len(train_set)
        train_loss.append(train_loss_ep)
        train_acc.append(train_acc_ep)
        with torch.no_grad():
            correct_val = 0
            for x,t in val_loader:
                z = model(x.to(device))
                J_val = loss(z, t.to(device))
                _, pred = torch.max(z, 1)
                correct_val += (pred == t.to(device)).sum().item()
                running_val_loss += J_val.item()*x.to(device).size(0)
            val_acc_ep = correct_val/len(val_set)
            val_loss_ep = running_val_loss/len(val_set)
            val_acc.append(val_acc_ep)
            val_loss.append(val_loss_ep)
            if val_acc_ep > best_val_acc:
                best_val_acc = val_acc_ep
                best_model_weights = copy.deepcopy(model.state_dict())
        scheduler.step()
        with torch.no_grad():
            correct_test = 0
            for x,t in test_loader:
                z = model(x.to(device))
                J_test = loss(z, t.to(device))
                _, pred = torch.max(z, 1)
                correct_test += (pred == t.to(device)).sum().item()
                running_test_loss += J_test.item()*x.to(device).size(0)
            test_acc_ep = correct_test/len(test_set)
            test_loss_ep = running_test_loss/len(test_set)
            test_acc.append(test_acc_ep)
            test_loss.append(test_loss_ep)
        print(optimizer.state_dict()['param_groups'][0]['lr'])
        print(f"Epoch {epoch+1}: train accuracy: {train_acc_ep:1.4f} validation accuracy: {val_acc_ep:1.4f} test accuracy: {test_acc_ep:1.4f}")
        print(f"Epoch {epoch+1}: train loss: {train_loss_ep:1.4f} validation loss: {val_loss_ep:1.4f} test accuracy: {test_loss_ep:1.4f}")
        print("-------------------------------------------------------------------")
    model.load_state_dict(best_model_weights)
    return model, {'train_loss': train_loss, 'train_acc':train_acc, 'val_loss': val_loss, 'val_acc':val_acc,
                   'test_loss':test_loss, 'test_acc':test_acc}
        
    
    
if __name__ == "__main__":
    torch.set_float32_matmul_precision('medium')
    model = AlexNet()
    best_model, metrics = train(model, num_epochs=100)
    
    metrics_list = [[key, value] for key, value in metrics.items()]

    fig = plt.figure(figsize = (15, 5))
    for idx in range(6):
        ax = fig.add_subplot(2, 3, idx + 1)
        ax.plot(metrics_list[idx][1])
        ax.set_title(metrics_list[idx][0])
    plt.savefig("DeepLearning/LR_Scheduler/metrics.png")
    plt.show()